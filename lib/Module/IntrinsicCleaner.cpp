//===-- IntrinsicCleaner.cpp ----------------------------------------------===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Passes.h"
#include <stdio.h>
#include "klee/Config/Version.h"
#if LLVM_VERSION_CODE >= LLVM_VERSION(3, 3)
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"

#else
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/InstrTypes.h"
#include "llvm/Instruction.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#if LLVM_VERSION_CODE >= LLVM_VERSION(3, 2)
#include "llvm/IRBuilder.h"
#else
#include "llvm/Support/IRBuilder.h"
#endif
#if LLVM_VERSION_CODE <= LLVM_VERSION(3, 1)
#include "llvm/Target/TargetData.h"
#else
#include "llvm/DataLayout.h"
#endif
#endif
#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Target/TargetLibraryInfo.h"

using namespace llvm;

namespace klee {

char IntrinsicCleanerPass::ID;

bool IntrinsicCleanerPass::runOnModule(Module &M) {
  bool dirty = false;
  for (Module::iterator f = M.begin(), fe = M.end(); f != fe; ++f)
    for (Function::iterator b = f->begin(), be = f->end(); b != be; ++b)
      dirty |= runOnBasicBlock(*b, M);
    if (Function *Declare = M.getFunction("llvm.trap"))
      Declare->eraseFromParent();
  return dirty;
}

bool IntrinsicCleanerPass::runOnBasicBlock(BasicBlock &b, Module &M) {
  bool dirty = false;
  bool block_split=false;
  
#if LLVM_VERSION_CODE <= LLVM_VERSION(3, 1)
  unsigned WordSize = TargetData.getPointerSizeInBits() / 8;
#else
  unsigned WordSize = DataLayout.getPointerSizeInBits() / 8;
#endif
  for (BasicBlock::iterator i = b.begin(), ie = b.end();
       (i != ie) && (block_split == false);) {
    IntrinsicInst *ii = dyn_cast<IntrinsicInst>(&*i);
    // increment now since LowerIntrinsic deletion makes iterator invalid.
    ++i;  
    if(ii) {
      switch (ii->getIntrinsicID()) {
      case Intrinsic::vastart:
      case Intrinsic::vaend:
        break;
        
        // Lower vacopy so that object resolution etc is handled by
        // normal instructions.
        //
        // FIXME: This is much more target dependent than just the word size,
        // however this works for x86-32 and x86-64.
      case Intrinsic::vacopy: { // (dst, src) -> *((i8**) dst) = *((i8**) src)
        Value *dst = ii->getArgOperand(0);
        Value *src = ii->getArgOperand(1);

        if (WordSize == 4) {
          Type *i8pp = PointerType::getUnqual(PointerType::getUnqual(Type::getInt8Ty(getGlobalContext())));
          Value *castedDst = CastInst::CreatePointerCast(dst, i8pp, "vacopy.cast.dst", ii);
          Value *castedSrc = CastInst::CreatePointerCast(src, i8pp, "vacopy.cast.src", ii);
          Value *load = new LoadInst(castedSrc, "vacopy.read", ii);
          new StoreInst(load, castedDst, false, ii);
        } else {
          assert(WordSize == 8 && "Invalid word size!");
          Type *i64p = PointerType::getUnqual(Type::getInt64Ty(getGlobalContext()));
          Value *pDst = CastInst::CreatePointerCast(dst, i64p, "vacopy.cast.dst", ii);
          Value *pSrc = CastInst::CreatePointerCast(src, i64p, "vacopy.cast.src", ii);
          Value *val = new LoadInst(pSrc, std::string(), ii); new StoreInst(val, pDst, ii);
          Value *off = ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 1);
          pDst = GetElementPtrInst::Create(pDst, off, std::string(), ii);
          pSrc = GetElementPtrInst::Create(pSrc, off, std::string(), ii);
          val = new LoadInst(pSrc, std::string(), ii); new StoreInst(val, pDst, ii);
          pDst = GetElementPtrInst::Create(pDst, off, std::string(), ii);
          pSrc = GetElementPtrInst::Create(pSrc, off, std::string(), ii);
          val = new LoadInst(pSrc, std::string(), ii); new StoreInst(val, pDst, ii);
        }
        ii->removeFromParent();
        delete ii;
        break;
      }

      case Intrinsic::uadd_with_overflow:
      case Intrinsic::usub_with_overflow:
      case Intrinsic::umul_with_overflow: {
        IRBuilder<> builder(ii->getParent(), ii);
        Function *F = builder.GetInsertBlock()->getParent();

        Value *op1 = ii->getArgOperand(0);
        Value *op2 = ii->getArgOperand(1);
        
        Value *result = 0;
        Value *overflow = 0;
        if (ii->getIntrinsicID() == Intrinsic::uadd_with_overflow){
          result = builder.CreateAdd(op1, op2);
          overflow = builder.CreateICmpULT(result, op1);
        } else if (ii->getIntrinsicID() == Intrinsic::usub_with_overflow){
          result = builder.CreateSub(op1, op2);
          overflow = builder.CreateICmpUGT(result, op1);
        } else if (ii->getIntrinsicID() == Intrinsic::umul_with_overflow){
          BasicBlock *entry = builder.GetInsertBlock();
          entry->setName(Twine(entry->getName(), "_umul_start"));
          BasicBlock *cont_of = entry->splitBasicBlock(builder.GetInsertPoint(),
                                                       "umul_end");
          BasicBlock *on_of = BasicBlock::Create(builder.getContext(),
                                                 "umul_overflow", F, cont_of);

          // remove the branch inserted by splitBasicBlock, we'll add our own
          entry->getTerminator()->eraseFromParent();
          builder.SetInsertPoint(entry);
          Value *no_overflow = builder.getFalse();
          Value *one = ConstantInt::getSigned(op1->getType(), 1);
          Value *op1_g1 = builder.CreateICmpUGT(op1, one);
          Value *op2_g1 = builder.CreateICmpUGT(op2, one);
          Value *may_of = builder.CreateAnd(op1_g1, op2_g1);
          builder.CreateCondBr(may_of, on_of, cont_of);

          builder.SetInsertPoint(on_of);
          uint64_t bit_size = op1->getType()->getPrimitiveSizeInBits();
          Value *uint_max = ConstantInt::get(op1->getType(),
                                             APInt::getMaxValue(bit_size));
          Value *div1 = builder.CreateUDiv(uint_max, op1);
          Value *overflow1 = builder.CreateICmpUGT(op2, div1);
          builder.CreateBr(cont_of);

          builder.SetInsertPoint(cont_of, cont_of->begin());
          PHINode *phi_of = builder.CreatePHI(no_overflow->getType(), 2);
          phi_of->addIncoming(overflow1, on_of);
          phi_of->addIncoming(no_overflow, entry);

          result = builder.CreateMul(op1, op2);
          overflow = phi_of;
          block_split = true;
	  /* my implementation
          Value *one = ConstantInt::getSigned(op1->getType(), 1);
          Value *op1_g1 = builder.CreateICmpUGT(op1, one);
          Value *op2_g1 = builder.CreateICmpUGT(op2, one);
          Value *may_of = builder.CreateAnd(op1_g1, op2_g1);
	  uint64_t bit_size = op1->getType()->getPrimitiveSizeInBits();
          Value *uint_max = ConstantInt::get(op1->getType(),
                                             APInt::getMaxValue(bit_size));
          Value *div1 = builder.CreateUDiv(uint_max, op1);
          Value *overflow1 = builder.CreateICmpUGT(op2, div1);
	  overflow = builder.CreateAnd(may_of, overflow1);
	  result = builder.CreateMul(op1, op2);*/
        }

        Value *resultStruct =
          builder.CreateInsertValue(UndefValue::get(ii->getType()), result, 0);
        resultStruct = builder.CreateInsertValue(resultStruct, overflow, 1);
        ii->replaceAllUsesWith(resultStruct);
        ii->removeFromParent();
        delete ii;
        dirty = true;
        break;
      }
      case Intrinsic::sadd_with_overflow:
      case Intrinsic::ssub_with_overflow:
      case Intrinsic::smul_with_overflow:{
        //refer to https://www.securecoding.cert.org/confluence/display/java/NUM00-J.+Detect+or+prevent+integer+overflow
        IRBuilder<> builder(ii->getParent(), ii);
      
        Value *op1 = ii->getArgOperand(0);
        Value *op2 = ii->getArgOperand(1);      
        Value *result = 0;
        Value *overflow = 0;
	//INT_MAX
	uint64_t bit_size = op1->getType()->getPrimitiveSizeInBits();
        Value *int_max = ConstantInt::get(op1->getType(),
                                             APInt::getSignedMaxValue(bit_size));
        Value *int_min = ConstantInt::get(op1->getType(),
                                             APInt::getSignedMinValue(bit_size));
	Value *zero = ConstantInt::getSigned(op1->getType(), 0);
	
        if (ii->getIntrinsicID() == Intrinsic::sadd_with_overflow){
          result = builder.CreateAdd(op1, op2);
	  Value *op2_gt0 = builder.CreateICmpSGT(op2, zero);  	  
	  Value *sub1 = builder.CreateSub(int_max, op2);
	  Value *test1 = builder.CreateICmpSGT(op1, sub1);
	  Value *overflow1 = builder.CreateAnd(op2_gt0, test1);
	  Value *sub2 = builder.CreateSub(int_min, op2);  
	  Value *test2 = builder.CreateICmpSLT(op1, sub2);
	  Value *overflow2 = builder.CreateAnd(builder.CreateNot(op2_gt0), test2);
	  
	  overflow = builder.CreateOr(overflow1, overflow2);
	  
        } else if (ii->getIntrinsicID() == Intrinsic::ssub_with_overflow){
          result = builder.CreateSub(op1, op2);
	  Value *op2_gt0 = builder.CreateICmpSGT(op2, zero);  	  
	  Value *add1 = builder.CreateAdd(int_min, op2);
          Value *test1 = builder.CreateICmpSLT(op1, add1);
	  Value *overflow1 = builder.CreateAnd(op2_gt0, test1);
	  Value *add2 = builder.CreateAdd(int_max, op2);
          Value *test2 = builder.CreateICmpSGT(op1, add2);
	  Value *overflow2 = builder.CreateAnd(builder.CreateNot(op2_gt0), test2);
	  
	  overflow = builder.CreateOr(overflow1, overflow2);       
	} else if (ii->getIntrinsicID() == Intrinsic::smul_with_overflow){          
          result = builder.CreateMul(op1, op2);
	  Value *op2_gt0 = builder.CreateICmpSGT(op2, zero); 
	  Value *div1 = builder.CreateSDiv(int_max, op2);
	  Value *div2 = builder.CreateSDiv(int_min, op2);
	  Value *may1 = builder.CreateICmpSGT(op1, div1);
	  Value *may2 = builder.CreateICmpSLT(op1, div2);
	  Value *test1 = builder.CreateOr(may1, may2);
          Value *overflow1 = builder.CreateAnd(op2_gt0, test1);	  
	  Value *None = ConstantInt::getSigned(op1->getType(), -1);
	  
	  Value *op2_ltN1 = builder.CreateICmpSLT(op2, None);
	  Value *may3 = builder.CreateICmpSGT(op1, div2);
	  Value *may4 = builder.CreateICmpSLT(op1, div1);
	  Value *test2 = builder.CreateOr(may3, may4);
	  Value *overflow2 = builder.CreateAnd(op2_ltN1, test2);
	  
	  Value *overflow3 = builder.CreateAnd(builder.CreateICmpEQ(op2, None), builder.CreateICmpEQ(op1, int_min));
	  
	  overflow = builder.CreateOr(overflow1, overflow2);
	  overflow = builder.CreateOr(overflow, overflow3);
	  
        }

        Value *resultStruct =
          builder.CreateInsertValue(UndefValue::get(ii->getType()), result, 0);
        resultStruct = builder.CreateInsertValue(resultStruct, overflow, 1);
        
        ii->replaceAllUsesWith(resultStruct);
        ii->removeFromParent();
        delete ii;
        dirty = true;
        break;
      }
      case Intrinsic::objectsize: {
        //determine the size of the object
	IRBuilder<> builder(ii->getParent(), ii);
	Value *result = ConstantInt::get(ii->getType(), 0);	;
	Value *op1 = ii->getArgOperand(0); //pointer
        Value *op2 = ii->getArgOperand(1); //true or false
	const TargetLibraryInfo *TLI = 0;
	uint64_t Size;
	if (getObjectSize(op1, Size, &DataLayout, TLI)){
	  result = ConstantInt::get(ii->getType(), Size);
	}
	else {
	  if(ConstantInt* CI=dyn_cast<ConstantInt>(op2)) {
 	     uint64_t DontKnow = CI->isZero() ? -1ULL : 0;	    
	     result = ConstantInt::get(ii->getType(), DontKnow);	
	  }
	} 
	ii->replaceAllUsesWith(result);
        ii->removeFromParent();
        delete ii;
        dirty = true;
	break;
      }
      case Intrinsic::fabs: {
	//llvm.fabs; compute the absolute value of a floating point number
        IRBuilder<> builder(ii->getParent(), ii);
        Function *F = builder.GetInsertBlock()->getParent();
        Value *op = ii->getArgOperand(0);
        BasicBlock *entry = builder.GetInsertBlock();
        entry->setName(Twine(entry->getName(), "_fabs_start"));

        BasicBlock *cont_of = entry->splitBasicBlock(builder.GetInsertPoint(),                                                      							"fabs_end");
        BasicBlock *cont_neg = BasicBlock::Create(builder.getContext(),
                                                 "fabs_negitive", F, cont_of);
        // remove the branch inserted by splitBasicBlock, we'll add our own
        entry->getTerminator()->eraseFromParent();
        builder.SetInsertPoint(entry);
       // Value *zero = ConstantFP::getZeroValueForNegation(op->getType());
	Value *zero = ConstantFP::get(op->getType(), 0);
        Value *is_neg = builder.CreateFCmpOLT(op, zero);
        builder.CreateCondBr(is_neg, cont_neg, cont_of);
        builder.SetInsertPoint(cont_neg);      
        Value *neg_op = builder.CreateFNeg(op);
        builder.CreateBr(cont_of);
        builder.SetInsertPoint(cont_of, cont_of->begin());
        PHINode *phi_of = builder.CreatePHI(ii->getType(), 2);
        phi_of->addIncoming(neg_op, cont_neg);
        phi_of->addIncoming(op, entry);

	block_split = true; //it's very important to set this flag to be true, otherwise will get stuck
        Value *result = phi_of;
        ii->replaceAllUsesWith(result);
        ii->removeFromParent();
        delete ii;	
        dirty = true;
	break;
      }
      case Intrinsic::dbg_value:
      case Intrinsic::dbg_declare:
        // Remove these regardless of lower intrinsics flag. This can
        // be removed once IntrinsicLowering is fixed to not have bad
        // caches.
        ii->eraseFromParent();
        dirty = true;
        break;

      case Intrinsic::trap: {
        // Intrisic instruction "llvm.trap" found. Directly lower it to
        // a call of the abort() function.
        Function *F = cast<Function>(
          M.getOrInsertFunction(
            "abort", Type::getVoidTy(getGlobalContext()), NULL));
        F->setDoesNotReturn();
        F->setDoesNotThrow();

        CallInst::Create(F, Twine(), ii);
        new UnreachableInst(getGlobalContext(), ii);

        ii->eraseFromParent();

        dirty = true;
        break;
      }
                    
      default:
        if (LowerIntrinsics)
          IL->LowerIntrinsicCall(ii);
        dirty = true;
        break;
      }
    }
  }

  return dirty;
}
}

//===-- Checks.cpp --------------------------------------------------------===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <stdio.h>
#include "Passes.h"
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
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DataLayout.h"
#else
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/InstrTypes.h"
#include "llvm/Instruction.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/LLVMContext.h"
#if LLVM_VERSION_CODE <= LLVM_VERSION(3, 1)
#include "llvm/Target/TargetData.h"
#else
#include "llvm/DataLayout.h"
#endif
#endif
#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
using namespace llvm;
using namespace klee;

char DivCheckPass::ID;

bool DivCheckPass::runOnModule(Module &M) { 
  Function *divZeroCheckFunction = 0;

  bool moduleChanged = false;
  
  for (Module::iterator f = M.begin(), fe = M.end(); f != fe; ++f) {
    for (Function::iterator b = f->begin(), be = f->end(); b != be; ++b) {
      for (BasicBlock::iterator i = b->begin(), ie = b->end(); i != ie; ++i) {     
          if (BinaryOperator* binOp = dyn_cast<BinaryOperator>(i)) {
          // find all [s|u][div|mod] instructions
          Instruction::BinaryOps opcode = binOp->getOpcode();
          if (opcode == Instruction::SDiv || opcode == Instruction::UDiv ||
              opcode == Instruction::SRem || opcode == Instruction::URem) {
            
            CastInst *denominator =
              CastInst::CreateIntegerCast(i->getOperand(1),
                                          Type::getInt64Ty(getGlobalContext()),
                                          false,  /* sign doesn't matter */
                                          "int_cast_to_i64",
                                          i);
            
            // Lazily bind the function to avoid always importing it.
            if (!divZeroCheckFunction) {
              Constant *fc = M.getOrInsertFunction("klee_div_zero_check", 
                                                   Type::getVoidTy(getGlobalContext()), 
                                                   Type::getInt64Ty(getGlobalContext()), 
                                                   NULL);
              divZeroCheckFunction = cast<Function>(fc);
            }

            CallInst * ci = CallInst::Create(divZeroCheckFunction, denominator, "", &*i);

            // Set debug location of checking call to that of the div/rem
            // operation so error locations are reported in the correct
            // location.
            ci->setDebugLoc(binOp->getDebugLoc());
            moduleChanged = true;
          }
        }
      }
    }
  }
  return moduleChanged;
}

char OvershiftCheckPass::ID;

bool OvershiftCheckPass::runOnModule(Module &M) {
  Function *overshiftCheckFunction = 0;

  bool moduleChanged = false;

  for (Module::iterator f = M.begin(), fe = M.end(); f != fe; ++f) {
    for (Function::iterator b = f->begin(), be = f->end(); b != be; ++b) {
      for (BasicBlock::iterator i = b->begin(), ie = b->end(); i != ie; ++i) {
          if (BinaryOperator* binOp = dyn_cast<BinaryOperator>(i)) {
          // find all shift instructions
          Instruction::BinaryOps opcode = binOp->getOpcode();

          if (opcode == Instruction::Shl ||
              opcode == Instruction::LShr ||
              opcode == Instruction::AShr ) {
            std::vector<llvm::Value*> args;

            // Determine bit width of first operand
            uint64_t bitWidth=i->getOperand(0)->getType()->getScalarSizeInBits();

            ConstantInt *bitWidthC = ConstantInt::get(Type::getInt64Ty(getGlobalContext()),bitWidth,false);
            args.push_back(bitWidthC);

            CastInst *shift =
              CastInst::CreateIntegerCast(i->getOperand(1),
                                          Type::getInt64Ty(getGlobalContext()),
                                          false,  /* sign doesn't matter */
                                          "int_cast_to_i64",
                                          i);
            args.push_back(shift);


            // Lazily bind the function to avoid always importing it.
            if (!overshiftCheckFunction) {
              Constant *fc = M.getOrInsertFunction("klee_overshift_check",
                                                   Type::getVoidTy(getGlobalContext()),
                                                   Type::getInt64Ty(getGlobalContext()),
                                                   Type::getInt64Ty(getGlobalContext()),
                                                   NULL);
              overshiftCheckFunction = cast<Function>(fc);
            }

            // Inject CallInstr to check if overshifting possible
            CallInst* ci =
#if LLVM_VERSION_CODE >= LLVM_VERSION(3, 0)
            CallInst::Create(overshiftCheckFunction, args, "", &*i);
#else
            CallInst::Create(overshiftCheckFunction, args.begin(), args.end(), "", &*i);
#endif
            // set debug information from binary operand to preserve it
            ci->setDebugLoc(binOp->getDebugLoc());
            moduleChanged = true;
          }
        }
      }
    }
  }
  return moduleChanged;
}


char FunctionCallPass::ID;

bool FunctionCallPass::runOnModule(Module &M) {
  Function *kleeMakeSymbolic = 0;
  Type *i8Ty = Type::getInt8Ty(getGlobalContext());
  //FIXME  ITY is an integer that can hold a pointer
  Type* ITy = Type::getInt64Ty(getGlobalContext());
  Type *i32Ty = Type::getInt32Ty(getGlobalContext());
  Type *i64Ty = Type::getInt64Ty(getGlobalContext());
  Type *i1Ty = Type::getInt1Ty(getGlobalContext());
  
  bool moduleChanged = false;

  std::vector<Type *> arg_type;
  arg_type.push_back(PointerType::getUnqual(i8Ty));
  arg_type.push_back(ITy);
  Function* memsetFn =  Intrinsic::getDeclaration(&M, Intrinsic::memset, arg_type);
  assert(memsetFn && "can not find Intrinsic: memset");
  Constant *fc = M.getOrInsertFunction("malloc", PointerType::getUnqual(i8Ty), i64Ty, NULL);
  Function* mallocFn = cast<Function>(fc);
  assert(mallocFn && "can not find function: malloc");
  
  DataLayout* datalayout = new DataLayout(&M); 

  for (Module::iterator f = M.begin(), fe = M.end(); f != fe; ++f) {
     if (!f->isDeclaration()){
      if(f->getName().endswith("_fake_main") || f->getName().equals("main"))
	continue;
      std::vector<llvm::Value*> args;
      std::string fName = f->getName().str();
//      printf("instrument a call to function: %s\n", fName.c_str());
      std::vector<LLVM_TYPE_Q Type*> fArgs;
      Function *fakeMain = Function::Create(FunctionType::get(Type::getVoidTy(getGlobalContext()), fArgs, false),
      			      GlobalVariable::ExternalLinkage, Twine(fName+"_fake_main"), &M);
      moduleChanged = true;
      BasicBlock *bb = BasicBlock::Create(getGlobalContext(), "entry", fakeMain);
      IRBuilder<> builder(bb, bb->begin());
      
      for(Function::arg_iterator ai = f->arg_begin(), ae = f->arg_end(); ai != ae; ++ai){
	// Lazily bind the function to avoid always importing it.
        if (!kleeMakeSymbolic) {
           Constant *fc = M.getOrInsertFunction("klee_make_symbolic",
						  Type::getVoidTy(getGlobalContext()),
                                                   PointerType::getUnqual(i8Ty),
                                                   Type::getInt64Ty(getGlobalContext()),
                                                   PointerType::getUnqual(i8Ty),
                                                   NULL);
           kleeMakeSymbolic = cast<Function>(fc);
        }	
	Type* tp = ai->getType(); 
	
	AllocaInst* arg_alloc = builder.CreateAlloca(tp);
	if(tp->isPointerTy()){
	    Type* tpp = tp->getPointerElementType();
	    if(!tp->getContainedType(0)->isPointerTy() && tpp->isSized()){//check whether tpp is sized	
		uint64_t size = datalayout->getTypeAllocSize(tpp);
		Value* alloc_size = ConstantInt::get(ITy, size);
		Instruction* mallocIn = builder.CreateCall(mallocFn, alloc_size);
		builder.CreateAlignedStore(builder.CreateBitCast(mallocIn, tp), arg_alloc, 8);
		//execute a call to memset
		Instruction* loadArg = builder.CreateAlignedLoad(arg_alloc, datalayout->getTypeAllocSize(tp));
		std::vector<Value* > memset_args;
		memset_args.push_back(builder.CreateBitCast(loadArg, PointerType::getUnqual(i8Ty)));
		memset_args.push_back(ConstantInt::get(i8Ty, 0));
		memset_args.push_back(alloc_size);
		memset_args.push_back(ConstantInt::get(i32Ty, 4));
		memset_args.push_back(ConstantInt::get(i1Ty, 0));
		builder.CreateCall(memsetFn, memset_args);
	    }
	}
	
	if(tp->isFloatTy()||tp->isDoubleTy()||tp->isIntegerTy()){//make scalar variable symbolic
	  std::vector<Value* > klee_args;
	  klee_args.push_back(builder.CreateBitCast(arg_alloc, 
						  kleeMakeSymbolic->getFunctionType()->getParamType(0)));
	  
	  klee_args.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()),
						   datalayout->getTypeAllocSize(tp)));
	  klee_args.push_back(builder.CreateGlobalStringPtr("arg_name"));
	  // Inject a call to klee_make_symbolic
	  builder.CreateCall(kleeMakeSymbolic, klee_args);
	}
	args.push_back(builder.CreateAlignedLoad(arg_alloc, datalayout->getTypeAllocSize(tp) ));
      }
      // Inject a call to the function
      builder.CreateCall(f, args);
      builder.CreateRetVoid();
      moduleChanged = true;
    }
  }
  return moduleChanged;
}

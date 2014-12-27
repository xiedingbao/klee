#include "testFunction.h"
void foo(struct st a){
    klee_make_symbolic(&a, sizeof(a), "a");
    if(a.i>0)
        printf("a is greater than 0\n");
    else
        printf("a is less than or equal to 0\n");
}
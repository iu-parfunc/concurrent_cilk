#include <stdio.h>
#include <cilk/concurrent_cilk.h>


typedef union {
  __cilkrts_ivar iv;
  int hed;
} union_var;

void union_var_clear(union_var v) {
  __cilkrts_ivar ivar;
  ivar = v.iv;  
  __cilkrts_ivar_clear(&ivar);
  v.iv = ivar;
}


union_var fun(){

  union_var uv;
  uv.hed = 1000;
return uv;
}


int main(int argc, char **argv) 
{
    printf("Simplest ivar test program...\n");
    union_var v;
    union_var_clear(v);
    v = cilk_spawn fun();
    printf("uvar hed is: %d uvar value is: %d uvar.header is: %d\n", v.hed, v.iv.__value, v.iv.__header);
    return 0;
}







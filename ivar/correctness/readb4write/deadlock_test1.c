#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

// The simplest IVar usage -- write before read.
//
void bar(__cilkrts_ivar *iv) {
  int i = (int) __cilkrts_ivar_read(iv);
  printf("reading i in bar: %d\n", i);
}


void fun() {
    __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);

    printf("reading...:\n");
     cilk_spawn __cilkrts_ivar_read(&iv);
    printf("Attempt to write ivar:\n");
    __cilkrts_ivar_write(&iv, (ivar_payload_t)39);

    cilk_spawn bar(&iv);
 
//    cilk_sync;
//
 //   printf("Ivar read successfully: %d\n", __cilkrts_ivar_read(&iv));
  //  if ((int) __cilkrts_ivar_read(&iv) != 39) abort();
  printf("returning from fun...this will pop the frame\n");
}

int main(int argc, char **argv) 
{
    printf("Simplest ivar deadlock test program...\n");
    cilk_spawn fun();
    cilk_sync;
    printf("after fun()\n");
    return 0;
}

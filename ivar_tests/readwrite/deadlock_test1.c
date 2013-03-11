#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

// The simplest IVar usage -- write before read.


void fun() {
    __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);

    printf("reading...:\n");
     cilk_spawn __cilkrts_ivar_read(&iv);
    printf("Attempt to write ivar:\n");
    cilk_spawn __cilkrts_ivar_write(&iv, (ivar_payload_t)39);
 
    cilk_sync;

    printf("Ivar read successfully: %d\n", __cilkrts_ivar_read(&iv));
    if ((int) __cilkrts_ivar_read(&iv) != 39) abort();
}

int main(int argc, char **argv) 
{
    printf("Simplest ivar deadlock test program...\n");
    cilk_spawn fun();
    return 0;
}

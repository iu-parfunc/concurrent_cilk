
// This variant runs a lot of other work on the side so that the replacement can issue steals.

#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>


// For access to Cilk RTS internals:
#include <internal/abi.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "timing.h"

#define DELAY (250*1000)
#define FIBINP 32

long long parfib(int n) 
{
    if (n <= 1) 
        return 1;
    else {
        long long x,y;
        x = cilk_spawn parfib(n-1);
        y = cilk_spawn parfib(n-2);
        cilk_sync;
        return x+y;
    }
}

void writer(__cilkrts_ivar* iv) 
{
    int val = 39;
    long long result = 5;

    printf("     Inside spawned writer... spawn spurious work\n");

    result = cilk_spawn parfib(FIBINP);

    printf("   spurious work finished or parent stolen (w=%d)\n", __cilkrts_get_tls_worker()->self); 

    __cilkrts_usleep(DELAY); // microseconds   
    __cilkrts_ivar_write(iv, (ivar_payload_t)val);
    printf("     Inside spawned writer... WRITE OF %d DONE (w=%d).\n", val, __cilkrts_get_tls_worker()->self);

    cilk_sync;
    printf("     Writer sync complete, fib result = %ld.\n", result);
}

void fun() {
    __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);
    printf("   Spawn to write ivar:\n");
    cilk_spawn writer(&iv);
    printf("   After spawn, wrote or stolen successfully, now perform ivar read:\n");

    unsigned long val = (unsigned long)__cilkrts_ivar_read(&iv);

    printf("   Ivar read successfully: %lu\n", val);
    cilk_sync;
    printf("   fun(): reached position AFTER cilk_sync\n");
    if (val != 39) { printf("TEST ERROR - BAD VALUE, %ld, EXPECTED 39 - ABORTING!\n", val); abort(); }
}

int main(int argc, char **argv) 
{
    printf("==== ivar5: simple test program...\n");
    fun();
    printf("==== ivar5: Finished.\n");
    return 0;
}


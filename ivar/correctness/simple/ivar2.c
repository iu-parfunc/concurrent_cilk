
// A parallel write that may happen after (in realtime) the read.

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
// For access to Cilk RTS internals:
#include <internal/abi.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

// TOGGLES:
//#define DELAY_WRITER // Working [2011.07.19]
//#define DELAY_READER // Infinite loops, nondetermistically [2011.07.19] 
//Both work [2012.4.27]

// #define DELAY (750*1000)
#define DELAY (750*1000)


void writer(__cilkrts_ivar* iv) {
    int val = 39;
    printf("     Inside spawned writer... (approx stack addr %p) sleeping for a bit\n", &val);
    __cilkrts_usleep(750 * 1000); // microseconds   
    __cilkrts_ivar_write(iv, (ivar_payload_t)val);
    printf("     Inside spawned writer... WRITE OF %d DONE (ivar %p).\n", val, iv);

    // [2011.07.19] DEBUGGING:  If I force this worker to get to the sync LAST then I get a segfault:
#ifdef DELAY_WRITER
    __cilkrts_usleep(DELAY); printf("  Writer done sleeping, now returning to sync point.\n");
#endif
}

void fun() {
     __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);

    printf("   Spawn to write ivar:\n");
    cilk_spawn writer(&iv);
    //cilk_sync;
    //writer(&iv);

    printf("   After spawn, wrote or stolen successfully, now read:\n");

    unsigned long val = (unsigned long)__cilkrts_ivar_read(&iv);
    //val = (unsigned long)__cilkrts_ivar_read(&iv); //this gets the right answer 4.22.2012 csz
    __cilkrts_worker* w_ = __cilkrts_get_tls_worker();
    printf("   Ivar (%p) read successfully: %lu w=%d\n",  &iv, val, w_->self);

    // [2011.07.19] Presently I'm crashing on the sync:
    struct __cilkrts_worker* w = __cilkrts_get_tls_worker();
    printf("fun(): Going to attempt Sync.  Current Cilk worker = %d\n", w->self);


    // [2011.07.19] DEBUGGING:  If I force this worker to get to the join LAST then I see the infinite loop behavior:
    // USUALLY -- it will still infrequently segfault.
#ifdef DELAY_READER
    __cilkrts_usleep(DELAY);   printf("fun(): Reader done sleeping, now sync\n");
#endif
    cilk_sync;
    printf("   fun(): reached position AFTER cilk_sync\n");

    if (val != 39) { printf("TEST ERROR - BAD VALUE, %ld, EXPECTED 39 - ABORTING!\n", val); abort(); }
}

int main(int argc, char **argv) 
{
    printf("==== ivar2: simple test program...\n");

    // cilk_spawn fun();
    fun();

    printf("==== ivar2: Finished.\n");
    return 0;
}

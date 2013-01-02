// A parallel write that may happen after (in realtime) the read.

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
// For access to Cilk RTS internals:
#include <cilk/abi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

// TOGGLES:
//#define DELAY_WRITER // Working [2011.07.19]
//#define DELAY_READER // Infinite loops, nondetermistically [2011.07.19] 
//Both work [2012.4.27]

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

unsigned long reader(__cilkrts_ivar* iv)
{
  return (unsigned long)__cilkrts_ivar_read(iv);
}

void fun() {
     __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);
     int x = 7;

    printf("   Spawn to read ivar:\n");

    cilk_spawn reader(&iv);
    
    int i = 23 + x;
    printf("   some contiguous worke done afte read but befor write: %d should be 30\n", i);
    printf("   After spawn, about to write\n");
    cilk_spawn writer(&iv);

    __cilkrts_worker* w_ = __cilkrts_get_tls_worker();
    unsigned long val;
    val = (unsigned long) cilk_spawn reader(&iv);
    printf("   Ivar (%p) read successfully: %lu double check of ivarstruct %lu w=%d\n",  &iv, val, iv.__value, w_->self);

    struct __cilkrts_worker* w = __cilkrts_get_tls_worker();
    printf("fun(): Going to attempt Sync.  Current Cilk worker = %d\n", w->self);

#ifdef DELAY_READER
    __cilkrts_usleep(DELAY);   printf("fun(): Reader done sleeping, now sync\n");
#endif
    cilk_sync;
    printf("   fun(): reached position AFTER cilk_sync\n");

    if (val != 39) { printf("TEST ERROR - BAD VALUE, %ld, EXPECTED 39 - ABORTING!\n", val); abort(); }
}

int main(int argc, char **argv) 
{
    printf("==== deadlock_test(iv2): simple test program...\n");
    fun();
    printf("==== deadlock_test(iv2): Finished.\n");
    return 0;
}


// This is variant of ivar2.c in which the IVar is heap allocated.
// See ivar2.c for additional details.

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

// TOGGLES:
// #define DELAY_WRITER // Working [2011.07.19]
// #define DELAY_READER // Infinite loops, nondetermistically [2011.07.19]
#define EXTRA_SPAWN

#define DELAY (750*1000)

void writer(__cilkrts_ivar* iv) {
    int val = 39;
    printf("     Inside spawned writer... sleeping for a bit\n");
    __cilkrts_usleep(750 * 1000); // microseconds   
    __cilkrts_ivar_write(iv, (void*)val);
    printf("     Inside spawned writer... WRITE OF %d DONE (ivar %p).\n", val, iv);

    // [2011.07.19] DEBUGGING:  If I force this worker to get to the sync LAST then I get a segfault:
#ifdef DELAY_WRITER
    __cilkrts_usleep(DELAY); printf("  Writer done sleeping, now returning to sync point.\n");
#endif
}

void fun() {
    __cilkrts_ivar* iv = malloc(sizeof(__cilkrts_ivar)); // Presently never freed.
    __cilkrts_ivar_clear(iv);

    printf("   Spawn to write ivar:\n");
    cilk_spawn writer(iv);
    printf("   After spawn, wrote or stolen successfully, now read:\n");

    volatile uintptr_t* tmp = (uintptr_t*)iv;
    printf("   Peeking[1/3] IVar (%p) before read: header %lu, payload %lu\n",  iv, tmp[0], tmp[1]);

    unsigned long val = (unsigned long)__cilkrts_ivar_read(iv);

    struct __cilkrts_worker* w_ = __cilkrts_get_tls_worker();
    printf("   Peeking[2/3] IVar (%p): header %lu, payload %lu\n",  iv, tmp[0], tmp[1]);
    printf("   Ivar (%p) read successfully: %lu w=%d\n",  iv, val, w_->self);
    printf("   Peeking[3/3] IVar (%p): header %lu, payload %lu\n",  iv, tmp[0], tmp[1]);

    unsigned long val2 = (unsigned long)__cilkrts_ivar_read(iv);
    struct __cilkrts_worker* w2 = __cilkrts_get_tls_worker();
    printf("   Here is a reread of IVar (%p): %lu  w=%d\n",  iv, val2, w2->self);

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
    printf("==== ivar2B: simple test program...\n");

#ifdef EXTRA_SPAWN
    cilk_spawn fun();
#else
    fun();
#endif

    printf("==== ivar2B: Finished.\n");
    return 0;
}

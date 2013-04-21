// A parallel write that may happen after (in realtime) the read.

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
// For access to Cilk RTS internals:
#include <cilk/abi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#define DELAY (750*1000)

void writer(__cilkrts_ivar* iv) {
    int val = 39;
    printf("     Inside spawned writer... (approx stack addr %p) sleeping for a bit\n", &val);
    __cilkrts_usleep(750 * 1000); // microseconds   
    __cilkrts_ivar_write(iv, (ivar_payload_t) val);
    printf("     Inside spawned writer... WRITE OF %d DONE (ivar %p).\n", val, iv);

#ifdef DELAY_WRITER
    __cilkrts_usleep(DELAY); printf("  Writer done sleeping, now returning to sync point.\n");
#endif
}

unsigned long reader(__cilkrts_ivar* iv) {
  return (unsigned long) __cilkrts_ivar_read(iv);
}

void fun() {
     __cilkrts_ivar iv;
    unsigned long val;

    __cilkrts_ivar_clear(&iv);
    val = (unsigned long) cilk_spawn __cilkrts_ivar_read(&iv);

#ifdef DELAY_READER
    __cilkrts_usleep(DELAY);   printf("fun(): Reader done sleeping, now sync\n");
#endif
    printf(" serial write of ivar\n");
    cilk_spawn writer(&iv);

    cilk_sync;
    struct __cilkrts_worker* w = __cilkrts_get_tls_worker();
    printf("   Ivar (%p) read successfully: %lu double check of ivarstruct %lu w=%d\n",  &iv, val, iv.__value, w->self);
    printf("fun(): Going to attempt Sync.  Current Cilk worker = %d\n", w->self);

    printf("   fun(): reached position AFTER cilk_sync\n");

    if (val != 39) { printf("TEST ERROR - BAD VALUE, %ld, EXPECTED 39 - ABORTING!\n", val); abort(); }
}

int main(int argc, char **argv) 
{
    printf("==== deadlock_test: simple test program...\n");
    fun();
    printf("==== deadlock_test: Finished.\n");
    return 0;
}

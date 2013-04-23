// This variant runs a lot of other work on the side so that the replacement can issue steals.

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

// For access to Cilk RTS internals:
#include <internal/abi.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#define DELAY (250*1000)
#define FIBINP 32

long long parfib(int n) 
{
  if (n <= 1) 
    return 1;
  else {
    long long x, y;
    x = cilk_spawn parfib(n-1);
    y = parfib(n-2);
    cilk_sync;
    return x+y;
  }
}


void writer(__cilkrts_ivar* iv) 
{
  int val = 39;
  long long result;

  printf("     Inside spawned writer... spawn spurious work\n");

  result = cilk_spawn parfib(FIBINP);

  printf("   spurious work finished or parent stolen (w=%d)\n", __cilkrts_get_tls_worker()->self); 

  __cilkrts_ivar_write(iv, (ivar_payload_t) val);

  printf("     Inside spawned writer... WRITE OF %d DONE (w=%d).\n", val, __cilkrts_get_tls_worker()->self);

  cilk_sync;

  printf("     Writer sync complete, fib result = %ld.\n", result);
}

void fun() {
  __cilkrts_ivar iv;
  __cilkrts_ivar_clear(&iv);

  printf("   spawning a read:\n");

  cilk_spawn __cilkrts_ivar_read(&iv);

  printf("   Spawn to write ivar:\n");
  cilk_spawn writer(&iv);

  struct __cilkrts_worker* w = __cilkrts_get_tls_worker();
  printf("fun(): Going to attempt Sync.  Current Cilk worker = %d\n", w->self);
  cilk_sync;

  printf("   Ivar read successfully: %lu  w=%d\n", __cilkrts_ivar_read(&iv), __cilkrts_get_tls_worker()->self);

  printf("   fun(): reached position AFTER cilk_sync\n");

  if ((int) __cilkrts_ivar_read(&iv) != 39) { 
    printf("TEST ERROR - BAD VALUE, %ld, EXPECTED 39 - ABORTING!\n", __cilkrts_ivar_read(&iv)); abort(); 
  }
}

int main(int argc, char **argv) 
{
  printf("==== ivar5: simple test program...\n");
  // both of these work, your choice which one you want to use
  //cilk_spawn fun();
  fun();

  printf("==== deadlock_test5: Finished.\n");
  return 0;
}

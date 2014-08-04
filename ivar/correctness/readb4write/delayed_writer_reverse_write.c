#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include "../simple/timing.h"

#define DELAY (750*1000)

void fun() {
  __cilkrts_ivar iv1;
  __cilkrts_ivar iv2;

  __cilkrts_ivar_clear(&iv1);
  __cilkrts_ivar_clear(&iv2);

  int i1 = cilk_spawn __cilkrts_ivar_read(&iv1);
  int i2 = cilk_spawn __cilkrts_ivar_read(&iv2);
  __cilkrts_usleep(DELAY); printf("  Writer done sleeping.\n");

  cilk_spawn __cilkrts_ivar_write(&iv2, (ivar_payload_t) 2);
  __cilkrts_ivar_write(&iv1, (ivar_payload_t) 1);

  cilk_sync;

  if (3 == (i1+i2)) {
    printf("sum of ivars is corect! (3)\n");
  } else  {
    printf("sum wrong!! \n");
  }

}

int main(int argc, char **argv) {
  cilk_spawn fun();
}


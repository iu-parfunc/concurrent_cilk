#include <stdio.h>
#include <internal/abi.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include "../simple/timing.h"

#define DELAY (950*1000)

void fun() {
  __cilkrts_ivar iv1;
  __cilkrts_ivar iv2;
  __cilkrts_ivar iv3;
  printf("iv1* %p iv2* %p iv3* %p\n", &iv1, &iv2, &iv3);
  printf("in fun sf %p\n", __cilkrts_get_tls_worker()->current_stack_frame);

  __cilkrts_ivar_clear(&iv1);
  __cilkrts_ivar_clear(&iv2);
  __cilkrts_ivar_clear(&iv3);

  int i1 = cilk_spawn __cilkrts_ivar_read(&iv1);
  __cilkrts_usleep(DELAY); 
  int i2 = cilk_spawn __cilkrts_ivar_read(&iv2);
  __cilkrts_usleep(DELAY);
  int i3 = cilk_spawn __cilkrts_ivar_read(&iv3);

  cilk_spawn __cilkrts_ivar_write(&iv1, (ivar_payload_t) 1);
  cilk_spawn __cilkrts_ivar_write(&iv2, (ivar_payload_t) 2);
  cilk_spawn __cilkrts_ivar_write(&iv3, (ivar_payload_t) 3);

  cilk_sync;

  if (6 == (i1+i2+i3)) {
    printf("sum of ivars is correct! (6)\n");
  } else  {
    printf("sum wrong!! \n");
  }

}

int main(int argc, char **argv) {
  //printf("above fun sf %p\n", __cilkrts_get_tls_worker()->current_stack_frame);
  fun();
  //printf("below fun sf %p\n", __cilkrts_get_tls_worker()->current_stack_frame);
  printf("<<<<<<<<<<<<<<< below fun\n");
}

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

  if (6 == (read_iv(&iv1) + read_iv(&iv2) + read_iv(&iv3))) {
    printf("sum of ivars is correct! (6)\n");
  } else  {
    printf("sum wrong!! \n");
  }
}

void bar() {
  __cilkrts_ivar iv1 = 0;
  int i1 = cilk_spawn read_iv(&iv1);
  write_iv(&iv1, 1);
  cilk_sync;
  printf("in bar...iv value %d (2)\n", ((int) read_iv(&iv1)) + i1);
}


void cux() {
  cilk_spawn bar();
  cilk_spawn fun();
  cilk_sync;
}

int main(int argc, char **argv) {
  //printf("above fun sf %p\n", __cilkrts_get_tls_worker()->current_stack_frame);
  //cilk_spawn cux();
  //printf("below cux!\n");
  //cilk_spawn fun();
  //printf("below fun sf %p\n", __cilkrts_get_tls_worker()->current_stack_frame);
  //cilk_spawn bar();
  //printf("<<<<<<<<<<<<<<< below bar\n");
  fun();
  printf("<<<<<<<<<<<<<<< below fun\n");
}

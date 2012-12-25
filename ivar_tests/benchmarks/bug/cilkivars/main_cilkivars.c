#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>


int reader(__cilkrts_ivar *iv) {
  printf("in reader with ivar: %p\n", iv);
  ivar_payload_t v =  __cilkrts_ivar_read(iv);
  return v;
}

void writer(__cilkrts_ivar *iv) {
  printf("in writer with ivar: %p\n", iv);
  __cilkrts_ivar_write(iv,5);
}


int main(void) {
  __cilkrts_ivar iv;
  __cilkrts_ivar_clear(&iv);

  //does not actually perfom spawn, and deadlocks
  //int var = cilk_spawn reader(&iv);

  //performs the spawn, completes
  int var;
  var = cilk_spawn reader(&iv);

  cilk_spawn writer(&iv);

  cilk_sync;
  printf("value: 5 == %d\n", var);
  return 0;
}

//@tests: are runtime internals working in both the slow path (ivar empty) and the fast path (ivar full) and that ivar write works.

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define TEST_VAL 39

void bar(ivar_t *iv) {
  int val = (int) read_iv(iv);
  if (val != TEST_VAL) { printf("TEST ERROR - BAD VALUE, %d, EXPECTED %d - ABORTING!\n", val, TEST_VAL); abort(); }
  printf("reading i in bar: %d\n", val);
}


void fun() {
    ivar_t iv;
    clear_iv(&iv);

    printf("read before write -slow path\n");
    //test the slow path -ivar is empty on read
    cilk_spawn read_iv(&iv);

    printf("write ivar\n");
    //serial write, now the ivar should be full
    write_iv(&iv, (ivar_payload_t) TEST_VAL);

    //test the fast path -ivar is full on read
    printf("read after write -fast path\n");
    cilk_spawn bar(&iv);
 
  printf("returning from fun...this will pop the frame\n");
}

int main(int argc, char **argv) 
{
  cilk_spawn fun();
  cilk_sync;
  printf("test complete\n");
  return 0;
}

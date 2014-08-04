//@tests: are runtime internals working for a two reads on a single ivar using the slow path for a read

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

    printf("1st read before write -slow path\n");
    //read on the slow path TWICE
    cilk_spawn read_iv(&iv);
    printf("2nd read before write -slow path\n");
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
  fun();
  printf("test complete\n");
  return 0;
}

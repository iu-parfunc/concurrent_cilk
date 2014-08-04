
//@tests: are runtime internals working for a two reads on a single ivar using the slow path for a read

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define TEST_VAL 39

void bar(ivar_t *iv1, ivar_t *iv2, ivar_t *iv3) {
  int val1, val2, val3;

  val1 = cilk_spawn read_iv(iv2);
  write_iv(iv1, (ivar_payload_t) TEST_VAL);
  val3 = (int) read_iv(iv3);
  val2 = read_iv(iv2);
  printf("success in bar %i\n", val1 == val2 == val3);
}

void fun() {
    ivar_t iv1, iv2, iv3;
    clear_iv(&iv1);
    clear_iv(&iv2);
    clear_iv(&iv3);
    int val1, val3;

    cilk_spawn bar(&iv1, &iv2, &iv3);
    val1 = read_iv(&iv1);
    write_iv(&iv2, (ivar_payload_t) TEST_VAL);
    val3 = read_iv(&iv2);
    write_iv(&iv3, (ivar_payload_t) TEST_VAL);
  printf("returning from fun...this will pop the frame\n");
}

int main(int argc, char **argv) 
{
  fun();
  printf("test complete\n");
  return 0;
}

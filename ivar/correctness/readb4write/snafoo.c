#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define PFIB_N 36
#define TEST_VAL 39

long pfib(int n) {
  if (n<2) return 1;

  long x;
  long y;
  x = cilk_spawn pfib(n-1);
  y =  pfib(n-2);

  cilk_sync;
  return x+y;
}


void foo(ivar_t *ivar_1)
{
  pfib(PFIB_N);
  write_iv(ivar_1, TEST_VAL);
}

void snafoo(ivar_t *ivar_1, ivar_t *ivar_2)
{
  int x;

  x = (int) read_iv(ivar_1);
  write_iv(ivar_2, TEST_VAL);
}

void quux(ivar_t *ivar_1, ivar_t *ivar_2)
{
  int x;

  cilk_spawn snafoo(ivar_1, ivar_2);
  x = read_iv(ivar_2);
}

void baz(ivar_t *ivar_1, ivar_t *ivar_2)
{
  int x, y;
  cilk_spawn foo(ivar_1);
  cilk_spawn quux(ivar_1, ivar_2);

  x = (int) read_iv(ivar_1);
  y = (int) read_iv(ivar_2);
}

int main(int argc, char **argv) 
{
  ivar_t ivar_1;
  ivar_t ivar_2;
  clear_iv(&ivar_1);
  clear_iv(&ivar_2);

  cilk_spawn baz(&ivar_1, &ivar_2);
  cilk_sync;
  printf("test complete\n");
  return 0;
}

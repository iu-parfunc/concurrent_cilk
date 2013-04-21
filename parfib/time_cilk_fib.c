#include <stdio.h>                     /* for printf() */
#include <assert.h>                    /* for assert() */
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

static long fib(void *arg_)
{
  long *n = (long *)arg_;

  if (*n < 2) {
    return *n;
  }

  long ret1, ret2;
  long n1 = *n - 1;
  long n2 = *n - 2;

  ret1 = cilk_spawn fib(&n1);
  ret2 = cilk_spawn fib(&n2);

  cilk_sync;

  return ret1 + ret2;
}

int main(int   argc,
    char *argv[])
{

  long n;
  if (argc>1)
    n = atoi(argv[1]);
  else
    n = 42;

  printf("Running fib of %d...\n", n);

  long ret = 0;
  ret = cilk_spawn fib(&n);
  cilk_sync;


  printf("Fib(%d) is %ld\n", n, ret);

  return 0;
}

/* vim:set expandtab */

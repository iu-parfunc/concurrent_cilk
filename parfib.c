#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <cilk/cilk.h>
#include "timer.h"
#include "cycle.h"

long pfib(int n) {
  if (n<2) return 1;

  long x;
  long y;
  x = cilk_spawn pfib(n-1);
  // We add a spawn here in order to match time_fib in qthreads
  y = cilk_spawn pfib(n-2);

  cilk_sync;
  return x+y;
}

int main(int argc, char** argv) {
  int n;
  long j;
  my_timer_t t;


  if (argc>1)
    n = atoi(argv[1]);
  else
    n = 42;

// passing a dummy argument through in order to warm things up (and keep them warm...)
  long ret;
  ret = pfib(n);

  TIMER_START(t);
  ticks t1 = getticks();
  j=pfib(n);
  ticks t2 = getticks();
  TIMER_STOP(t);

  printf("%d\t%f\t%lf\n", n, TIMER_EVAL(t), elapsed(t2,t1));
  //printf("%d\t%f\n", n, TIMER_EVAL(t));
  return 0;
}

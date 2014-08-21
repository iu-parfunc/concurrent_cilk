#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/ivar.h>
#include <cilk/cilk_api.h>
#include <timer.h>
#include <cycle.h>
#include "wavefront.hpp"

using namespace std;

int main(int argc, char **argv) {
  matrix *mat;
  my_timer_t t;
  int dim = 64;
  ticks start, end;
  unsigned long long sum;

  if (argc > 1) dim = atoi(argv[1]);

  mat = new matrix(dim);

  TIMER_START(t);
  start = getticks(); 
  dbgprintf("starting rows and columns traversal!\n", NULL);
  mat->traverse_by_par_row_and_column();
  end = getticks();
  TIMER_STOP(t);

  cilk_sync;
  sum = mat->sum();
  printf("%d\t%f\t%lf\n", dim, TIMER_EVAL(t), elapsed(end,start));
  return 0;

}

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <cilk/cilk.h>
#include <cilk/cilk_undocumented.h>
#include "timer.h"
#include "ivar_tests/common/cycle.h"


#define READIV(IV, TYPE) ((TYPE)__cilkrts_ivar_read(IV))
#define WRITEIV(IV,VAL) (__cilkrts_ivar_write(IV,(ivar_payload_t)VAL))
#define CLEARIV(IV) (__cilkrts_ivar_clear(IV))

typedef __cilkrts_ivar ivar;


void pfib(ivar *iv, int n){
  long res;
   ivar iv1;
   ivar iv2;
  CLEARIV(&iv1);
  CLEARIV(&iv2);

  if(n < 2) { 
    WRITEIV(iv, 1);
    return;
  }

  cilk_spawn pfib(&iv1, n-1);
  cilk_spawn pfib(&iv2, n-2);

  res = READIV(&iv1, long) + READIV(&iv2, long);
  WRITEIV(iv, res);
}

int main(int argc, char** argv) {
  int n;
  long j;
  my_timer_t t;


  if (argc>1)
    n = atoi(argv[1]);
  else
    n = 42;

  ivar iv;
  CLEARIV(&iv);
  
  ivar dummy;
  CLEARIV(&dummy);

// passing a dummy argument through in order to warm things up (and keep them warm...)
//  long ret;
 // pfib(&dummy, n);
  //ret = (long) __cilkrts_ivar_read(&dummy);

// Ok, we've warmed up now, let actually run it
  TIMER_START(t);
  ticks t1 = getticks();
  pfib(&iv, n);
  j =  (long) __cilkrts_ivar_read(&iv);
  ticks t2 = getticks();
  TIMER_STOP(t);

  printf("%d\t%f\t%lf\t%d\n", n, TIMER_EVAL(t), elapsed(t2,t1), __cilkrts_get_total_workers());
  //printf("%d\t%f\n", n, TIMER_EVAL(t));
  __cilkrts_dump_stats();
  return 0;
}

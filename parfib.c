//#include "cilk/cilk.h"
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <cilk/cilk.h>
#include "timer.h"
//#define CILK_IVARS 



#define READIV(IV, TYPE) ((TYPE)__cilkrts_ivar_read(IV))
#define WRITEIV(IV,VAL) (__cilkrts_ivar_write(IV,(ivar_payload_t)VAL))
#define CLEARIV(IV) (__cilkrts_ivar_clear(IV))

typedef __cilkrts_ivar ivar;

long pfib(int n) {
  if (n<2) return 1;

  long x;
  long y;
  x = cilk_spawn pfib(n-1);
  // We add a spawn here in order to match time_fib in qthreads
  y = cilk_spawn pfib(n-2);

  //this runs serially! compiler bug?
  //long x = pfib(n-1);
  //long y = cilk_spawn pfib(n-2);

  cilk_sync;
  return x+y;
}
/*
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
*/
int main(int argc, char** argv) {
  int n;
  long j;
  my_timer_t t;


  if (argc>1)
    n = atoi(argv[1]);
  else
    n = 42;

 //ivar iv;
 //CLEARIV(&iv);

  //printf("Running pfib of %d...\n", n);
  /*
#ifdef CILK_IVARS
printf("defined\n");
__cilkrts_ivar iv;
#else
printf("not defined\n"):
#endif
fflush(stdout);
*/
  TIMER_START(t);
 // j =  (long) cilk_spawn __cilkrts_ivar_read(&iv);
  //pfib(&iv, n);
  //cilk_sync;
  j=pfib(n);
  TIMER_STOP(t);


//  printf("%f\n",TIMER_EVAL(t));
  printf("%d\t%f\n", n, TIMER_EVAL(t));
//  printf("Fib(%d) is %ld time taken was %f\n", n, j, TIMER_EVAL(t));
  //printf("Fib(%d) is %ld\n", n, pfib(n));
  return 0;
}

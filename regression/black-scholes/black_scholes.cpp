#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <cilk/cilk.h> 
#include "../../../timer.h"
#include "../../common/cycle.h"

#define NUM_OPTIONS 20*1000*1000
#define SEED  7676767 // or whatever your heart so desires....
#define SL 10.0f
#define SH 50.0f
#define KL 10.0f
#define KH 50.0f
#define TL 0.2f
#define TH 2.0f

#define PI 3.141592653589793238462643

using namespace std;

static double N (const double& z){
  if (z > 6.0)
    return 1.0;
  if (z < -6.0)
    return 0.0;
  double b1 = 0.31938153;
  double b2 = -0.356563782;
  double b3 = 1.781477937;
  double b4 = -1.821255978;
  double b5 = 1.330274429;
  double p = 0.2316419;
  double c2 = 0.3989423;
  double a = fabs(z);
  double t = 1.0/(1.0 + a*p);
  double b = c2*exp((-z)*(z/2.0));
  double n = ((((b5*t+b4)*t+b3)*t+b2)*t+b1)*t;
  n = 1.0 - b*n;
  if(z < 0.0 )
    n = 1.0 - n;
  return n;
};

double call_black_scholes (double S, double K, double r, double sigma, double time){
  double time_sqrt = sqrt (time);
  double d1 = (log (S/K) + r*time)/(sigma*time_sqrt) + 0.5*sigma*time_sqrt;
  double d2 = d1 - (sigma*time_sqrt);
  return S*N (d1) - K*exp (-r*time)*N (d2);
}

double put_black_scholes (double S, double K, double r, double sigma, double time){
  double time_sqrt = sqrt (time);
  double d1 = (log (S/K) + r*time)/(sigma*time_sqrt) + 0.5*sigma*time_sqrt;
  double d2 = d1 - (sigma*time_sqrt);
  return K*exp (-r*time)*N (-d2) - S*N (-d1);
}

// QUESTION: If we remove the cilk_spawns (one spawn is fine) from inside the cilk_for, we get a segfault. Why is this?
void call_put_black_scholes (double S [NUM_OPTIONS], double K [NUM_OPTIONS],
           double r, double sigma, double time [NUM_OPTIONS],
           double call [NUM_OPTIONS], double put [NUM_OPTIONS]){
  cilk_for (int i = 0; i < NUM_OPTIONS; i++){
    call [i] =  cilk_spawn call_black_scholes (S [i], K [i], r, sigma, time [i]);
    put [i]  =  cilk_spawn put_black_scholes  (S [i], K [i], r, sigma, time [i]);
  }
}

void create_input_data (double S [NUM_OPTIONS], double K [NUM_OPTIONS],
      double time [NUM_OPTIONS], double call [NUM_OPTIONS], double put [NUM_OPTIONS]){
  srand (SEED);
  int i;
  for (i = 0; i < NUM_OPTIONS; i++){
    S [i] = (double)rand ()/(double)RAND_MAX*(double) (SH - SL) + (double)SL;
    K [i] = (double)rand ()/(double)RAND_MAX*(double) (KH - KL) + (double)KL;
    time [i] = (double)rand ()/(double)RAND_MAX*(double) (TH - TL) + (double)TL;
    //call [i] = 0.0;
    //put [i] = 0.0;
  }
  // We cant memset the others, since we'll then just end up with utter junk
  // but we can memset these
  memset (call, 0.0, NUM_OPTIONS*sizeof (double));
  memset (put, 0.0, NUM_OPTIONS*sizeof (double));
}

double sum (double arr [NUM_OPTIONS]){
  int i;
  double ret;
  for (i = 0; i < NUM_OPTIONS; i++)
    ret += arr [i];
  return ret;
}

int main (){
  double* S = new double [NUM_OPTIONS];
  double* K = new double [NUM_OPTIONS];
  double* time = new double [NUM_OPTIONS];
  double* call = new double [NUM_OPTIONS];
  double* put = new double [NUM_OPTIONS];
  
    my_timer_t t;
  double r = 0.10;
  double sigma = 0.30;

  // create data
  printf ("=============== Creating Input Data ===================\n");
  create_input_data (S, K, time, call, put); // look at this...
  printf( "=============== Created Input Data ====================\n");
  
  printf ("======= Starting to run call_put_black_scholes ========\n");
  TIMER_START (t);
  ticks start = getticks();
  call_put_black_scholes (S, K, r, sigma, time, call, put);
  ticks end = getticks();
  TIMER_STOP (t);
  printf("===== Finished call_put_black_scholes. Summing =========\n");
  double sum_call = cilk_spawn sum (call);
  double sum_put = cilk_spawn sum (put);
  cilk_sync;// making sure were done
  //printf ("Execution took %f seconds, sum_call is: %f, sum_put is: %f\n", TIMER_EVAL (t), sum_call, sum_put);
  printf ("Execution took %f seconds, %lf cycles. sum_call is: %f, sum_put is: %f, total size is: %lu\n", TIMER_EVAL(t), elapsed(end,start), sum_call, sum_put, NUM_OPTIONS);

  return 0;
}


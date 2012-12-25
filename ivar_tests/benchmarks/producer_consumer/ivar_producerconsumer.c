// the equivalent qthread benchmark can be found in:
//  /u/parfunc/parfunc/opt/bin/qthread-1.8/test/benchmarks/generic/time_syncvar_producer_consumer.c

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include "../event/event_cilk.h"
//#include <qthread/qtimer.h> // for qtimer stuff

/*TODO:
 */


size_t ITERATIONS = 1000000;
size_t MAXPARALLELISM = 256;

event_data_t *IBUFF;
uint64_t **IVtable; // need to change this. WRONG used to be aligned_t

double *total_sending_time;
double *total_roundtrip_time;
double *total_p1_sending_time;
double *total_p2_sending_time;

uint64_t incrementme = 0;// used to be aligned_t

static uint64_t IV_consumer(event *arg){


  uint64_t pong = 0;

  pong = event_wait_nodeps(arg->self);
  if(pong != 1) {
    iprintf("ERROR: pong = %u\n", (unsigned)pong);
    assert(pong == 1);
  }
  return pong;
}

static uint64_t IV_producer(event *arg) {

  uint64_t ping = 1;

  event_fire(arg->self, &ping); // might want to deref
  return ping;
}

static int IV_producerloop(event *arg) {

  unsigned int offset = (unsigned int)(intptr_t)arg;
  uint64_t timer = 0;
  unsigned int i;

  for (i = 0; i < ITERATIONS; i++) {

    qtimer_start(sending[offset][timer]);
    event_fire((IBUFF + offset)->self, &timer);
    timer = timer ? 0 : 1;
  }
  return 0;
} 


static int IV_consumerloop(event *arg) {

  unsigned int offset = (unsigned int)(intptr_t)arg;
  uint64_t timer = 0;
  unsigned int i;

  for (i = 0; i < ITERATIONS; i++) {
    // add in timer stop here
    timer = event_wait_nodeps((IBUFF + offset)->self);
    qtimer_stop(sending[offset][timer]);
    total_sending_time[offset] += qtimer_secs(sending[offset][timer]);
  }
  return 0;
}

static int IV_player2(event *arg){

  unsigned int offset = (unsigned int)(intptr_t)arg;
  uint64_t paddle = 0;
  unsigned int i;


  for (i = 0; i < ITERATIONS; i++) {

    paddle = event_wait_nodeps(IVtable[offset]->self);
    qtimer_stop(sending[offset][0]);
    total_p1_sending_time[offset] += qtimer_secs(sending[offset][0]);
    qtimer_start(sending[offset][1]);
    event_fire((IVtable[offset] +1)->self, &paddle);
  }
  return 0;
}

static int IV_player1(event *arg) {

  unsigned int offset = (unsigned int)(intptr_t)arg;
  uint64_t paddle = 1;
  unsigned int i;
  qtimer_t roundtrip_timer = qtimer_create();

  /* serve */
  qtimer_start(sending[offset][0]);
  qtimer_start(roundtrip_timer);

  event_fire(IVtable[offset]->self, &paddle);

  for (i = 0; i < ITERATIONS; i++) {

    paddle = event_wait_nodeps((IVtable[offset] + 1)->self);
    qtimer_stop(sending[offset][1]);
    qtimer_stop(roundtrip_timer);

    total_roundtrip_time[offset] += qtimer_secs(roundtrip_timer);
    total_p2_sending_time[offset] += qtimer_secs(sending[offset][1]);
    if (i + 1 < ITERATIONS) {
      qtimer_start(sending[offset][0]);
      qtimer_start(roundtrip_timer);
      event_fire(IVtable[offset]->self, &paddle);
    }
  }
  qtimer_destroy(roundtrip_timer);
  return 0;
}

static char *human_readable_rate(double rate)
{
  static char readable_string[100] = { 0 };
  const double GB = 1024 * 1024 * 1024;
  const double MB = 1024 * 1024;
  const double kB = 1024;

  if (rate > GB) {
    snprintf(readable_string, 100, "(%.1f GB/s)", rate / GB);
  } else if (rate > MB) {
    snprintf(readable_string, 100, "(%.1f MB/s)", rate / MB);
  } else if (rate > kB) {
    snprintf(readable_string, 100, "(%.1f kB/s)", rate / kB);
  } else {
    memset(readable_string, 0, 100 * sizeof(char));
  }
  return readable_string;
}

int main(void) {


  qtimer_t timer = qtimer_create();
  double rate;
  unsigned int i;
  uint64_t rets[MAXPARALLELISM];
  event_init();
  CHECK_VERBOSE();// do we need this?? look at it
  if (!verbose) {
    return 0;
  }
  NUMARG(ITERATIONS, "ITERATIONS");
  NUMARG(MAXPARALLELISM, "MAXPARALLELISM");

  IBUFF = calloc(MAXPARALLELISM, sizeof(event_data_t)); // pointer??
  IVtable = calloc(MAXPARALLELISM, sizeof(uint64_t *)); //used to be aligned_t 
  sending = malloc(MAXPARALLELISM * sizeof(qtimer_t *));
  total_sending_time = malloc(MAXPARALLELISM * sizeof(double));
  total_roundtrip_time = malloc(MAXPARALLELISM * sizeof(double));
  total_p1_sending_time = malloc(MAXPARALLELISM * sizeof(double));
  total_p2_sending_time = malloc(MAXPARALLELISM * sizeof(double));

  for (i = 0; i < MAXPARALLELISM; i++) {
  
    IBUFF + i = event_create();
    sending[i] = malloc(2 * sizeof(qtimer_t));
    sending[i][0] = qtimer_create();
    sending[i][1] = qtimer_create();
    IVtable[i] = malloc(sizeof(uint64_t) * 2);// used to be aligned_t
    
    // need to add:
    //  qthread_empty(&(IVtable[i][0]));
    //  qthread_empty(&(IVtable[i][1]));
    // equivalents
  }
  printf("Testing producer consumer\n");

  printf("\tSingle send and receive: ");
  qtimer_start(timer);

  rets = cilk_spawn IV_consumer((event *)IBUFF);

  cilk_spawn IV_producer((event *)IBUFF);

  event_wait_nodeps(rets->self);
  qtimer_stop(timer);

  printf("%19g secs\n", qtimer_secs(timer));
  rate = sizeof(uint64_t) / qtimer_secs(timer);// aligned_t
  printf("\t = throughput: %29g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  printf("\tParallel single IV send/receive: ");

  qtimer_start(timer);
  for(i = 0; i <MAXPARALLELISM; i++) {
  
    rets + i = cilk_spawn IV_consumer((event *)(IBUFF + i));

    cilk_spawn IV_producer((event *)(IBUFF + i));
  }
  for(i=0; i < MAXPARALLELISM; i++){
  
    event_wait_nodeps((rets + i)->self);
  }

  qtimer_stop(timer);

  printf("%10g secs (%u parallel)\n", qtimer_secs(timer),
      (unsigned)MAXPARALLELISM);
  rate = (MAXPARALLELISM * sizeof(uint64_t)) / qtimer_secs(timer); // aligned_t
  printf("\t = throughput: %29g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  memset(total_sending_time, 0, sizeof(double) * MAXPARALLELISM);


// IV producer.consumer loop
  printf("\tIV producer/consumer loop: ");
  qtimer_start(timer);
  rets = cilk_spawn IV_consumerloop((event *)NULL);

  cilk_spawn IV_producerloop((evnet *)NULL);

  event_wait_nodeps(rets->self);

  qtimer_stop(timer);

  printf("%16g secs (%u iterations)\n", qtimer_secs(timer),
      (unsigned)ITERATIONS);
  printf("\t - total sending time: %21g secs\n", total_sending_time[0]);
  iprintf("\t + external average time: %18g secs\n",
      qtimer_secs(timer) / ITERATIONS);
  iprintf("\t + internal average time: %18g secs\n",
      total_sending_time[0] / ITERATIONS);
  printf("\t = message throughput: %21g msgs/sec\n",
      ITERATIONS / total_sending_time[0]);
  rate = (ITERATIONS * sizeof(uint64_t)) / total_sending_time[0];// aligned_T
  printf("\t = data throughput: %24g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  memset(total_sending_time, 0, sizeof(double) * MAXPARALLELISM);

  // now at line 215

  // parallel IV producer/consumer loops
  printf("\tParallel IV producer/consumer loop: ");

  qtimer_start(timer);
  for(i = 0; i < MAXPARALLELISM; i++){
  
    rets + i = cilk_spawn IV_consumerloop((event *)(intptr_t)i);
  }
  for(i = 0; i < MAXPARALLELISM; i++) {
  
    cilk_spawn IV_producerloop((event *)(intptr_t)i);
  }
  for(i = 0; i < MAXPARALLELISM; i++) {
  
    event_wait_nodeps((rets + i)->self);
  }
  qtimer_stop(timer);

  for (i = 1; i < MAXPARALLELISM; i++) {
    total_sending_time[0] += total_sending_time[i];
  }
  printf("%6g secs (%u-way %u iters)\n", qtimer_secs(timer),
      (unsigned)MAXPARALLELISM, (unsigned)ITERATIONS);
  printf("\t - total sending time: %21g secs\n", total_sending_time[0]);
  iprintf("\t + external average time: %18g secs\n",
      qtimer_secs(timer) / (ITERATIONS * MAXPARALLELISM));
  iprintf("\t + internal average time: %18g secs\n",
      total_sending_time[0] / (ITERATIONS * MAXPARALLELISM));
  printf("\t = message throughput: %21g msgs/sec\n",
      (ITERATIONS * MAXPARALLELISM) / qtimer_secs(timer));
  rate =
    (ITERATIONS * MAXPARALLELISM * sizeof(uint64_t)) /
    qtimer_secs(timer); // aligned_T
  printf("\t = data throughput: %24g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  memset(total_p1_sending_time, 0, sizeof(double) * MAXPARALLELISM);
  memset(total_p2_sending_time, 0, sizeof(double) * MAXPARALLELISM);
  memset(total_roundtrip_time, 0, sizeof(double) * MAXPARALLELISM);

  // IV ping-pong

  printf("\tIV ping-pong loop: ");
  qtimer_start(timer);
  rets = cilk_spawn IV_player2((event *)NULL); 

  cilk_spawn IV_player1((event *)NULL);

  event_wait_nodeps(rets->self);
  qtimer_stop(timer);

  printf("%24g secs (%u round trips)\n", qtimer_secs(timer),
      (unsigned)ITERATIONS);
  printf("\t - total rtts: %29g secs\n", total_roundtrip_time[0]);
  printf("\t - total sending time: %21g secs\n",
      total_p1_sending_time[0] + total_p2_sending_time[0]);
  iprintf("\t + external avg rtt: %23g secs\n",
      qtimer_secs(timer) / ITERATIONS);
  iprintf("\t + internal avg rtt: %23g secs\n",
      total_roundtrip_time[0] / ITERATIONS);
  iprintf("\t + average p1 sending time: %16g secs\n",
      total_p1_sending_time[0] / ITERATIONS);
  iprintf("\t + average p2 sending time: %16g secs\n",
      total_p2_sending_time[0] / ITERATIONS);
  iprintf("\t + average sending time: %19g secs\n",
      (total_p1_sending_time[0] +
       total_p2_sending_time[0]) / (ITERATIONS * 2));
  /* each rt is 2 messages, thus: */
  printf("\t = messaging throughput: %19g msgs/sec\n",
      (ITERATIONS * 2) / total_roundtrip_time[0]);
  /* each rt is 1 message of uint64_t bytes each, thus: */
  rate = (ITERATIONS * sizeof(uint64_t)) / total_roundtrip_time[0]; // aligned_t
  printf("\t = data roundtrip tput: %20g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  /* each send is 1 messsage of uint64_t bytes, thus: */
  rate = (ITERATIONS * sizeof(uint64_t)) / total_p1_sending_time[0];// aligned_t
  printf("\t = p1 hop throughput: %22g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  rate = (ITERATIONS * sizeof(uint64_t)) / total_p2_sending_time[0];// aligned_T
  printf("\t = p2 hop throughput: %22g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  rate =
    (ITERATIONS * 2 * sizeof(uint64_t)) / (total_p1_sending_time[0] +
        total_p2_sending_time[0]); // aligned_t
  printf("\t = data hop throughput: %20g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  memset(total_p1_sending_time, 0, sizeof(double) * MAXPARALLELISM);
  memset(total_p2_sending_time, 0, sizeof(double) * MAXPARALLELISM);
  memset(total_roundtrip_time, 0, sizeof(double) * MAXPARALLELISM);

  // line 272

  // Parallel ping-pong loop
  printf("\tParallel IV ping-pong loop: ");
    qtimer_start(timer);

  // maybe change to a cilk_for?
  for(i = 0; i < MAXPARALLELISM; i++){
  
    rets + i = cilk_spawn IV_player2((event *)(intptr_t)i);
  }
  for(i = 0; i < MAXPARALLELISM; i++) {
  
    cilk_spawn IV_player1((event *)(intptr_t)i);
  }
  for(i = 0; i < MAXPARALLELISM; i++){
  
    event_wait_nodeps((rets + i)->self);
  }
  qtimer_stop(timer);

  for (i = 1; i < MAXPARALLELISM; i++) {
    total_roundtrip_time[0] += total_roundtrip_time[i];
    total_p1_sending_time[0] += total_p1_sending_time[i];
    total_p2_sending_time[0] += total_p2_sending_time[i];
  }
  printf("%15g secs (%u-way %u rts)\n", qtimer_secs(timer),
      (unsigned)MAXPARALLELISM, (unsigned)ITERATIONS);
  printf("\t - total rtts: %29g secs\n", total_roundtrip_time[0]);
  printf("\t - total sending time: %21g secs\n",
      total_p1_sending_time[0] + total_p2_sending_time[0]);
  iprintf("\t + external avg rtt: %23g secs\n",
      qtimer_secs(timer) / (MAXPARALLELISM * ITERATIONS));
  iprintf("\t + internal avg rtt: %23g secs\n",
      total_roundtrip_time[0] / (MAXPARALLELISM * ITERATIONS));
  iprintf("\t + average p1 sending time: %16g secs\n",
      total_p1_sending_time[0] / (MAXPARALLELISM * ITERATIONS));
  iprintf("\t + average p2 sending time: %16g secs\n",
      total_p2_sending_time[0] / (MAXPARALLELISM * ITERATIONS));
  iprintf("\t + average sending time: %19g secs\n",
      (total_p1_sending_time[0] +
       total_p2_sending_time[0]) / (MAXPARALLELISM * ITERATIONS * 2));
  /* each rt is 2 messages, thus: */
  printf("\t = messaging throughput: %19g msgs/sec\n",
      (MAXPARALLELISM * ITERATIONS * 2) / total_roundtrip_time[0]);
  /* each rt is 1 message of uint64_t bytes each, thus: */
  rate =
    (MAXPARALLELISM * ITERATIONS * sizeof(uint64_t)) /
    total_roundtrip_time[0];// aligned_t
  printf("\t = data roundtrip tput: %20g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  /* each send is 1 messsage of uint64_t bytes, thus: */
  rate =
    (MAXPARALLELISM * ITERATIONS * sizeof(uint64_t)) /
    total_p1_sending_time[0];//aligned_t
  printf("\t = p1 hop throughput: %22g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  rate =
    (MAXPARALLELISM * ITERATIONS * sizeof(uint64_t)) /
    total_p2_sending_time[0];// aligned_t
  printf("\t = p2 hop throughput: %22g bytes/sec %s\n", rate,
      human_readable_rate(rate));
  rate =
    (MAXPARALLELISM * ITERATIONS * 2 * sizeof(uint64_t)) /
    (total_p1_sending_time[0] + total_p2_sending_time[0]);//aligned_t
  printf("\t = data hop throughput: %20g bytes/sec %s\n", rate,
      human_readable_rate(rate));

  qtimer_destroy(timer);
  for (i = 0; i < MAXPARALLELISM; i++) {
    qtimer_destroy(sending[i][0]);
    qtimer_destroy(sending[i][1]);
    free(sending[i]);
    free(IVtable[i]);
  }
  free((void*)IBUFF);
  free(IVtable);
  free(sending);
  free(total_sending_time);
  free(total_roundtrip_time);
  free(total_p1_sending_time);
  free(total_p2_sending_time);

  return 0;
}

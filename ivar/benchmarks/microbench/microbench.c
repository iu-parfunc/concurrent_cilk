#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../common/cycle.h"
#include "../../../timer.h"

#define ALLOC_EMPTY_IVARS(size) calloc(size, sizeof(__cilkrts_ivar))

void writer(__cilkrts_ivar *array, long num_fibers){

  int i;
  //printf("inside spawned writer... WRITING\n");
  for(i = 0; i < num_fibers; i++){
  __cilkrts_ivar_clear(&array[i]);
  unsigned long num = 1000+i;
  __cilkrts_ivar_write(&(array[i]), (ivar_payload_t) num);
  }
  //printf("inside spawned writer.. ALL WRITES DONE\n");
}

void read_one(int i, __cilkrts_ivar *iv){
 long val;
  val = (long)  __cilkrts_ivar_read(iv); 
  //printf("read %d with value %d\n", i, val);
}

void readers(__cilkrts_ivar *array, long num_fibers) {
  int i;
  for(i = 0; i < num_fibers; i++){
    cilk_spawn read_one(i, &array[i]);
  }
}

int main(int argc, char **argv){
  //printf("===== Microbench many blocking ======\n");

  ticks start, end;
  my_timer_t t;
  long long num_fibers;
  if(argc == 2){
    num_fibers = atoi(argv[1]);
  } else {
  num_fibers = 1000;
  }
  __cilkrts_ivar *all_ivars = ALLOC_EMPTY_IVARS(num_fibers);
  if(NULL == all_ivars) perror("CALLOC failed to allocate array\n");

  //printf("creating %d blocked fibers\n", num_fibers);


  //printf("created array of ivars\n");

  TIMER_START(t);
  start = getticks();
  cilk_spawn writer(all_ivars, num_fibers);
  cilk_spawn readers(all_ivars, num_fibers);
  cilk_sync;
  end = getticks();
  TIMER_STOP(t);


  //printf("all ivars read successfully\n");

  int i;
  long long sum = 0;
  for(i = 0; i < num_fibers; i++){
    sum += (long) __cilkrts_ivar_read(&all_ivars[i]);
  }
  //printf("sum of all values: %ld\n", sum);

  long long expected = 0;
  for(i = 0; i < num_fibers; i++) {
    expected += 1000 + i;
  }

  free(all_ivars);
  if(sum != expected){
    fprintf(stderr, "BAD SUM!!\n");
    abort();
    return 1;
  } else {
    //printf("SUM CORRECT\n");
    printf("%lu\t%f\t%lf\n", num_fibers, TIMER_EVAL(t), elapsed(end,start));
    return 0;
  }
}


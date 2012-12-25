// NOT CORRECT
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <cilk/abi.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h> // for later on...

long long consumer(__cilkrts_ivar *iv){
  long long i;
  i = (long long) cilk_spawn __cilkrts_ivar_read(iv); // in case we are reading before writing. try taking this out to see what happens.

  return i;
}

void producer(__cilkrts_ivar *iv, long long i){
  __cilkrts_ivar_clear(iv);
  __cilkrts_ivar_write(iv, (ivar_payload_t)i);
}

int main(int argc, char **argv){
  
  long long num_fibers;
  if(argc == 2){
    num_fibers = atoi(argv[1]);
  } else {
    num_fibers = 2;
  }

  printf("======= Ivar Producer consumer test (throughput) =====\n");

  printf("== Initializing single ivar\n");
  __cilkrts_ivar iv;
  __cilkrts_ivar_clear(&iv);
  printf("== Ivar initialized\n");

  printf("==== Spinning up producer and consumer loops =====\n");

  printf("== Spinning up consumer loop\n");

  long long sum = 0;
  long long  g = 0;
  int j;
  for(j = 0; j < num_fibers; j++){
  g  = cilk_spawn consumer(&iv);
  sum += g;
  g = 0;
  }
  
  printf("== Spinning producer loop\n");
  
  int i;// shouldnt these be long long ?
  for(i = 0; i < num_fibers; i++){
    cilk_spawn producer(&iv, i);
  }


  long long expected;
  int k;
  expected = 0;
  for(k = 0; k < num_fibers; k++){
    expected += k;
  }

  printf("==== Finished producer and consumer loops. Sum is: %ld ======\n", sum);

  if(sum != expected){
    fprintf(stderr, "Bad sum!!");
    return 1;
  } 
  else printf("====== SUM CORRECT ======\n");

}


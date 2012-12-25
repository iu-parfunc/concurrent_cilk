#include <stdio.h>                     /* for printf() */
#include <stdlib.h>                    /* for strtol() */
#include <unistd.h>
#include <assert.h>                    /* for assert() */
#include <qthread/qthread.h>
#include <qthread/qloop.h>
#include <qthread/qtimer.h>

#define num_fibers 1

static aligned_t writer(void *array /* , long num_fibers */){

  int i = 0;

  printf("inside spawned...WRITING\n");
  for (i = 0; i < num_fibers; i++){

    //array[i] = SYNCVAR_INITIALIZER;
    qthread_syncvar_empty(&(array[i]));
    uint64_t num = 1000 + i;
    qthread_syncvar_writeEF_const(&(array[i]), &num);
  }
  printf("inside spawned writer.. ALL WRITES DONE\n");
}

static aligned_t read_one(/*int i,*/ void *sv){

  printf("in read one!!\n");
  uint64_t val;
  qthread_syncvar_readFF(&val, &sv);
  printf("read with value %d\n", val);
}

static aligned_t readers(void *array /*, long num_fibers */){

  printf("spawning readers!!\n");
  int i;
  syncvar_t t;
  for(i = 0; i < num_fibers; i++){
//    t = array[i];
    qthread_fork_syncvar(read_one, &(array[i]), &(array[i]));
 //read_one(&t);
  }
}

int main(int argc, char **argv){
  /*
     printf("===== Microbench many blocking ======\n");
     long long num_fibers;
     if(argc == 2){
     num_fibers = atoi(argv[1]);
     } else {
     num_fibers = 1000;
     }
     */
  printf("creating %d blocked fibers\n", num_fibers);
  assert(qthread_initialize() == 0);// make sure it initializes properly
  syncvar_t *all_syncvars = (syncvar_t *) calloc(num_fibers, sizeof(syncvar_t));

  //syncvar_t *all_syncvars_ret = (syncvar_t *) calloc(num_fibers, sizeof(syncvar_t));
 
  printf("created array of ivars\n");
  //writer(all_syncvars);
  qthread_fork(writer, all_syncvars, NULL);
  qthread_yield();
  printf("SPAWNING READERS\n");
 // readers(all_syncvars);
  qthread_fork(readers, all_syncvars, NULL);

  printf("all ivars read successfully\n");

  int i;
  uint64_t ret;
  long long sum = 0;
  for(i = 0; i < num_fibers; i++){
    qthread_syncvar_readFF(&ret, &(all_syncvars[i]));
  printf("reading... %d => %d\n", i, ret);
    sum += ret;
  }
  printf("sum of all values: %ld\n", sum);

  long long expected = 0;
  for(i = 0; i < num_fibers; i++) {
    expected += 1000 + i;
  }

  free(all_syncvars);
  if(sum != expected){
    fprintf(stderr, "BAD SUM!!\n");
    return 1;
  } else {
    printf("SUM CORRECT\n");
    return 0;
  }
}

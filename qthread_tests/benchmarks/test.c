#include <stdio.h>                     /* for printf() */
#include <stdlib.h>                    /* for strtol() */
#include <unistd.h>
#include <assert.h>                    /* for assert() */
#include <qthread/qthread.h>
#include <qthread/qloop.h>
#include <qthread/qtimer.h>

uint64_t writer(void *sv){

  uint64_t val;
  printf("in writer\n");
  int i;
  i = 1000;
  qthread_syncvar_writeEF(&sv, &i);
  qthread_syncvar_readFF(&val, &sv);
  return val;
}

uint64_t reader(void *sv){

  uint64_t val;
  printf("in reader\n");
  qthread_syncvar_readFF((syncvar_t *)val, sv);
  return val;

}

int main(void){


  qthread_initialize();
  printf("starting\n");

  syncvar_t sv;

  uint64_t val;
  //sv = SYNCVAR_INITIALIZER;
  //qthread_fork(writer, (void *)sv, val);
  //qthread_fork(reader, (void *)sv, val);
  printf("in writer\n");
  int i;
  i = 1000;
  qthread_syncvar_writeEF(&sv, &i);
  qthread_syncvar_readFF(&val, &sv);
  printf("returned with %llu\n", val);
  return 0;
}

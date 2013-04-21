#include "../event_cilk.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cilk/cilk.h>

int reader(event *e1) {
  uint64_t sum = ((event_data_t *) event_wait(e1->self))->u64;

  int i = e1->ndeps-1;
  for(i; i >= 0; i--)
    sum += ((event_data_t *) e1->deps[i])->u64;
  
  return sum;
}

void writer(event *e, uint64_t *num) {
  event_fire(e->self, (event_data_t *) num);
}


void input_writer(event *e1, event *e) {
  printf("enter a number: ");
  char buf[256];
  memset(&buf,0,256);
  if(gets(buf) !=0){
  uint64_t *num = (uint64_t *) malloc(sizeof(uint64_t));
  *num = (uint64_t) atoi(buf);
  event *e2 = event_create();
  event_ctl(e1->self, e->self, ADD);
  event_ctl(e1->self, e2->self, ADD);
  event_fire(e->self, (event_data_t *) num);
  printf("number is: %d\n", *num);
  input_writer(e1, e2);
  }
}
/*
void input_writer(event *e) {
  printf("enter a number: ");
  char buf[256];
  memset(&buf,0,256);
  gets(buf);
  uint64_t *num = (uint64_t *) malloc(sizeof(uint64_t));
  *num = (uint64_t) atoi(buf);
  event_fire(e->self, (event_data_t *) num);

}
*/
int main(void) {

  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;
  event_init();
  event *e1 = event_create();
  event *e2 = event_create();
  event *e3 = event_create();

  event_ctl(e1->self, e2->self, ADD);
  event_ctl(e1->self, e3->self, ADD);

  var = cilk_spawn reader(e1);

  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e2, &v2);
  cilk_spawn input_writer(e1,e3);

  cilk_sync;
  printf("value: = %d\n", var);

  return 0;
}
/*
int main(void) {

  uint64_t v1 = 39;
  uint64_t v2 = 1;
  int var;
  int i;
  event_init();
  event *e1 = event_create();
  event *e2 = event_create();
  event *e3 = event_create();
  event *ee = event_create();


  event_ctl(e1->self, e2->self, ADD);
  event_ctl(e1->self, e3->self, ADD);
  event_ctl(ee->self, e3->self, ADD);

  i = cilk_spawn reader(ee);
  cilk_spawn writer(ee, &var);
  var = cilk_spawn reader(e1);
  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e2, &v2);
  cilk_spawn input_writer(e3);


  cilk_sync;
  printf("value: = %d\n", i);

  return 0;
}
*/

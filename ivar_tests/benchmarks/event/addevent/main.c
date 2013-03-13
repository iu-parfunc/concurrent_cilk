#include "../event_cilk.h"
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>

int reader(event *e1) {
  printf("reader: waiting...\n");
  printf("e1: %p\n", e1);
  uint64_t sum = ((event_data_t *) event_wait(e1->self))->u64;
  printf("reader: caught event. continuing...\n");
  int i = e1->ndeps-1;

  for(i; i >= 0; i--)
    sum += ((event_data_t *) e1->deps[i])->u64;
  
  return sum;
}

void writer(event *e, uint64_t *num) {
  printf("writer: firing event\n");
  printf("event: %p eid: %d\n", e, e->self);
  event_fire(e->self, (event_data_t *) num);
}

int main(void) {

  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;
  event_init();
  event *e1 = event_create();
  event *e2 = event_create();
  event *e3 = event_create();
  printf("e1: %p\n", e1);

  event_ctl(e1->self, e2->self, ADD);
  event_ctl(e1->self, e3->self, ADD);

  var = cilk_spawn reader(e1);

  cilk_spawn writer(e2, &v2);
  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e3, &v3);

  cilk_sync;
  printf("value: 45 == %d\n", var);

  return 0;
}

#include "../event.h"
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>

using namespace event_cilk;

int reader(event *e){
  printf("reader: waiting...\n");
  uint64_t sum = e.get();
  printf("reader: caught event. continuing...\n");
  int i = e.deps() - 1;
  
  for(i; i >= 0; i --)
    sum += e.get_dep_value(i);

  return sum;
}

void writer(event *e, uint64_t *num){
  printf("writer: firing event\n");
  e.fulfill(num);
}

int main(){
  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;

  event *e1;
  event *e2;
  event *e3;

  e1.add_event_dep(e2);
  e1.add_event_dep(e3);

  var = cilk_spawn reader(e1);


  cilk_spawn writer(e2, &v2);
  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e3, &v3);


  cilk_sync;
  printf("value: 45 == %d\n", var);

  return 0;



}

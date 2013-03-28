#include "../event.h"
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

using namespace cilk_event;

//template<typename T>
int reader(event_c<uint64_t> e){
  printf("reader: waiting...\n");
  uint64_t sum = e.get();
  printf("reader: caught event. continuing...\n");
  int i = e.deps() - 1;
  
  for(i; i >= 0; i --)
    sum += e.get_dep_value(i);

  return sum;
}
//template<typename T>
void writer(event_c<uint64_t> e, uint64_t num){
  printf("writer: fulfilling event %p\n", e);
  e.fulfill(num);
}

int main(){
  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;

  event_c<uint64_t> e1; 
  event_c<uint64_t> e2;
  event_c<uint64_t> e3;

  e1.add_event_dep(e2);
  e1.add_event_dep(e3);

  var = reader(e1);


  writer(e2, v2);
  writer(e1, v1);
  writer(e3, v3);


  //cilk_sync;
  printf("value: 45 == %d\n", var);

  return 0;



}

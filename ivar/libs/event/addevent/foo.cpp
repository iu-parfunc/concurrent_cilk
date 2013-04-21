#include "../event.h"
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

using namespace cilk_event;



int main(){
  event_c<int> e1;
  e1.fulfill(3);
  printf("got: %d\n", (int)e1.get());
  return 0;
}

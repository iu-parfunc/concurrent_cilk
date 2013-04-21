#include <stdio.h>
#include "lockfree_queue.h"
#include <stdlib.h>

int main() {
  init_queue();

  int i = 10000;
  int *j;
  local_block_t *l = init_thread();
  int sum = 0, sum1 =0;

  for(;i>0; i--) {
    enqueue(l,(void *) &i);
  }

  for(;i<10000; i++) {
    sum1 += i;
    j = (int *) dequeue(l);
    sum += *j;
  }
  printf("sum: expected: %d, got: %d\n", sum1, sum);

}

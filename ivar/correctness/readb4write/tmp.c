#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

void bar(int volatile*volatile var) {
  printf("sleeping in bar\n");
  sleep(2);
  printf("bar returning %i\n", *var);
}

void fun() {
    volatile int var = 1;
    printf("entering fun\n");
    cilk_spawn bar(&var);
    printf("returning from fun\n");
}

void cux() {
  printf("entering cux\n");
  int buf[1024];
  int i = 0, sum = 0;
  memset(&buf, 2, 1024);
  for(i=0; i<1024; i++) {
    sum += buf[i];
  }
  printf("cux value: %i\n", sum);
  printf("returning from cux\n");
}

int main(int argc, char **argv) 
{
  fun();
  // cux squashes fun's stack frames. 
  cilk_spawn cux();
  printf("test complete\n");
  return 0;
}

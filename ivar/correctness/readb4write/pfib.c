#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>

#define PFIB_N 36

long pfib(int n) {
  if (n<2) return 1;

  long x;
  long y;
  x = cilk_spawn pfib(n-1);
  y =  pfib(n-2);

  cilk_sync;
  return x+y;
}

int main(int argc, char **argv) 
{
  pfib(PFIB_N);
  printf("test complete\n");
  return 0;
}

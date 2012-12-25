#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>


int foo(int i) {
  int v = i*i;
  return v;
}

int bar(int i) {
  int v = i+i;
  return v;
}

int main(void) {
  int i = 5;

  int var1;
  int var2;
  var1 = cilk_spawn foo(i);
  var2 = cilk_spawn bar(i);

  cilk_sync;
  printf("value: 35 == %d\n", var1+var2);
  return 0;
}

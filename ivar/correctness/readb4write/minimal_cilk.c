#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define TEST_VAL 39


void fun() {
  printf("2");
}

int main(int argc, char **argv) 
{
  cilk_spawn fun();
  return 0;
}

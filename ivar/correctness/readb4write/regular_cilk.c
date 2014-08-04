//@test: the fast path only 

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define TEST_VAL 39


void fun() {
    int i;

    i = 39;

    if (i != 39) abort();
    printf("var read successfully: %d\n", i);
}

int main(int argc, char **argv) 
{
    cilk_spawn fun();
    cilk_sync;
    return 0;
}

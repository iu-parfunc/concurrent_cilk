//@test: the fast path only 

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>

#define TEST_VAL 39

void fun() {
    ivar_t iv;
    int i;
    clear_iv(&iv);

    //fill the ivar before the read
    write_iv(&iv, (ivar_payload_t) TEST_VAL);

    //read on the fast path
    i = read_iv(&iv);
 
    if (i != 39) abort();
    printf("Ivar read successfully: %d\n", i);
}

int main(int argc, char **argv) 
{
    cilk_spawn fun();
    cilk_sync;
    return 0;
}

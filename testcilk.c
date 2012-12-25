#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <pin.H>
void fun() {
    __cilkrts_ivar iv;
    __cilkrts_ivar_clear(&iv);

    printf("Attempt to write ivar:\n");
    __cilkrts_ivar_write(&iv, (ivar_payload_t)39);
    printf("Wrote successfully, now read:\n");

    int val = (int)__cilkrts_ivar_read(&iv);
    printf("Ivar read successfully: %d\n", val);
    if (val != 39) abort();
}

int main(int argc, char **argv) 
{
    printf("Simplest ivar test program...\n");
    cilk_spawn fun();
    return 0;
}

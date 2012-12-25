
// This uses the higher level C++ api.

#include <unistd.h>
#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/ivar.h>

void writer(ivar<int>& iv) {
    printf("     Inside spawned writer... sleeping for a bit\n");
    usleep(250 * 1000); // microseconds

    iv.put(39);

    printf("     Inside spawned writer... WRITE DONE.\n");
}

void fun() {
    ivar<int> iv;

    printf("   Spawn to write ivar:\n");
    cilk_spawn writer(iv);

    printf("   After spawn, wrote or stolen successfully, now read:\n");

    int val = iv.get();
    printf("   Ivar read successfully: %d\n", val);
}

int main(int argc, char **argv) 
{
    printf("==== ivar3 test program...\n");

    cilk_spawn fun();

    return 0;
}

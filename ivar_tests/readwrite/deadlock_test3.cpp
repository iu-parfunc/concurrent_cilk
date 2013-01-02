// This uses the higher level C++ api.

#include <unistd.h>
#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/ivar.h>

void writer(ivar<int>& iv) {
    printf("     Inside spawned writer... sleeping for a bit\n");
    __cilkrts_usleep(250 * 1000); // microseconds

    iv.put(39);

    printf("     Inside spawned writer... WRITE DONE.\n");
}

void fun() {
    ivar<int> iv;

    printf("spawning read:\n");
    cilk_spawn iv.get();
    printf("   Spawn to write ivar:\n");
    cilk_spawn writer(iv);

    cilk_sync;

    printf("   Ivar read successfully: %d\n", iv.get());
}

int main(int argc, char **argv) 
{
    printf("==== deadlock_test3 test program...\n");

    cilk_spawn fun();

    return 0;
}

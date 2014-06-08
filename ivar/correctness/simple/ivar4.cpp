
// This test exercises ALL template specializations of the ivar<> class.  

#include <cilk/cilk.h>
#include <cilk/ivar.h>
#include <unistd.h>
#include <stdio.h>
#include "timing.h"

struct tup {
    int x,y,z,w;
};

class bundle
{
 public:
    ivar<int>  n;
    ivar<int*> p;
    ivar<tup>  t;

    void put() 
    {
        n.put(33);
        p.put(new int());
        t.put( tup() );
    }
    
    void get() 
    {
        n.get();
        p.get();
        t.get();
        printf("All three read.\n");
    }
};

void writer(bundle& b) 
{
    printf("     Inside spawned writer... sleeping for a bit\n");
    __cilkrts_usleep(250 * 1000); // microseconds
    b.put();
    printf("     Inside spawned writer... WRITE DONE.\n");
}

void fun() {
    bundle b;
    printf("   Spawn function to write ivars:\n");
    cilk_spawn writer(b);
    printf("   After spawn, wrote or stolen successfully, now read:\n");

    b.get();
    printf("   Ivars read successfully.\n");
    cilk_sync;
    printf("   Cilk_sync completed successfully..\n");
}


int main(int argc, char **argv) 
{
    printf("==== ivar4 test program...\n");

    cilk_spawn fun();
    return 0;
}

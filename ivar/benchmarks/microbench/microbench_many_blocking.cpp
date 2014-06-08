

// This microbenchmark uses IVars in a perverse way, creating many many blocked fibers.

#define CILK_IVARS

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <cilk/concurrent_cilk.h>
#include <cilk/cilk.h>
#include <cilk/ivar.h>

#include <vector>

using namespace std;

// int num_fibers = 1000;
int num_fibers = 100; // Default
// int num_fibers = 5;


void writer(vector< ivar<int> > & iv) {
    printf("====     Inside spawned writer... sleeping for a bit\n");
//    usleep(1000 * 1000); // microseconds

    for(int i=0; i<num_fibers; i++)
       iv[i].put(1000 + i);

    printf("====     Inside spawned writer... ALL WRITES DONE.\n");
}

void read_one(int i, ivar<int>* ptr) {
    int val = ptr->get();
    // printf("READ pos %d = %d\n", i, val);
}

void readers(vector< ivar<int> > & iv) 
{
    for(int i=0; i<num_fibers; i++) 
    {
        // This would normally be a cilk_for in practice, but that wouldn't create behavior as perverse.
        cilk_spawn read_one( i, &iv[i] ); // [anon spawn would be nice here...]
    }
}

int main(int argc, char **argv) 
{
    printf("==== microbench_many_blocking test program...\n");

    if (argc == 2) {
       num_fibers = atoi(argv[1]);
    }
    printf("==== Creating %d blocked fibers.\n", num_fibers);
    vector< ivar<int> > all_ivars( num_fibers );
    printf("==== Created array of ivars.\n");

    cilk_spawn writer(all_ivars);
    cilk_spawn readers(all_ivars);
    cilk_sync;

    printf("====   All ivars read successfully.\n");

    long long sum = 0;
    for(int i=0; i<num_fibers; i++)
      sum +=  all_ivars[i].get();

    printf("====   Sum of all values: %ld\n", sum);


    // sum $ map (+1000) [0.. 10000-1] == 59995000
    // Oracle for my common test values:
    long long expected = 0;
    for(int i=0; i<num_fibers; i++) expected += 1000+i;

    if ( sum != expected )
    {
        fprintf(stderr, "Bad sum!\n"); 
        return 1;
    }
    else printf(" (Sum correct.)\n");
 
    return 0;
}


// A very simple test of the Michael & Scott lock free queue implementation.

#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
// #include <cilk/cilk_stub.h>

// Careful, this has to be before concurrent_queue.h below:
#include <internal/abi.h>

// A little surgery here.  Any DBG messages will swamp this test.
#  undef  IVAR_DBG
#  define IVAR_DBG 1
#define QUEUE_ELEMTY long long
#define __cilkrts_malloc malloc
#define __cilkrts_free   free
#include <cilk/concurrent_queue.h>

// By default pump 1M elements through 4 pair producers/consumers (4M total)
// Currently [2011.07.31] this is about twice as fast with lockfree queues.
int numelems   = 1000000;
int numthreads = 8;


// We malloc each element and send its pointer.
void producer(__cilkrts_stack_queue* q) {
    long long i;
    for(i=0; i<numelems; i++) {
        long long* ptr = (long long*)malloc(sizeof(long long*));
        *ptr = i;
        enqueue_paused_stack(q, ptr);
    }
}

// We free on the consumer side.  Should use TBB malloc for this.
long long consumer(__cilkrts_stack_queue* q) {
    int i=0;
    long long sum = 0;
    while(i<numelems) {
       QUEUE_ELEMTY* ptr = dequeue_paused_stack(q);
       // printf("Dequeued!: %ld\n", *ptr);
       if (ptr) {
           i++;
           sum += *ptr;
           free(ptr);
       } else {
           printf(".");
           fflush(stdout);
       }
       // Otherwise we got nothing this time... keep going.
    }
    return sum;
}

int main(int argc, char** argv) 
{
    int i; 
    long long sum, *arr;

    __cilkrts_stack_queue* q;

    printf("Usage: %s numelements numthreads\n", argv[0]);
    if (argc > 3) {
        abort();
    }
    if (argc > 2)  numelems   = atoi(argv[1]);
    if (argc == 3) numthreads = atoi(argv[2]);
    // ----------------------------------------

    arr = (long long*)malloc(sizeof(long long) * (numthreads/2));

    q = make_stack_queue();
    printf("Q created and initialized: %p\n",q);

    printf("Starting %d producers and consumers, each will pass %d elements.\n", numthreads/2, numelems);
    for (i=0; i<numthreads/2; i++) 
       cilk_spawn producer(q);

    for (i=0; i<numthreads/2; i++) 
       arr[i] = cilk_spawn consumer(q);

    cilk_sync;
    printf("All producers and consumers finished.\n");

    sum = 0;
    for (i=0; i<numthreads/2; i++) 
        sum += arr[i];

    printf("Total: %ld\n", sum);

    // Default parameters should result in: 1999998000000
    if ( numelems == 1000000 && numthreads == 8 && sum != 1999998000000) {
        printf("ERROR: ANSWER NOT CORRECT, EXPECTING %ld\n", 1999998000000);
        fflush(stdout);
        abort();
    }
}


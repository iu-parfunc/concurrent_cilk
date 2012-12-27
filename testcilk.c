// filename: test_cilkplus.cpp
// compile: g++ test_cilkplus.cpp -lcilkrts

#include <stdio.h>
#include <unistd.h>

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

void task(int i) {
    printf("task: %d, workder id: %d\n", i, __cilkrts_get_worker_number());
    fflush(stdout);
    sleep(1);
}

int main() {
    for(int i=0;i<10;i++)
      cilk_spawn task(i);
    cilk_spawn task(-1);
    cilk_sync;
    return 0;
}

/* compile and result:
#g++ test_cilkplus.cpp -lcilkrts
#./a.out 
task: 0, workder id: 0
task: 1, workder id: 1
task: 2, workder id: 2
task: 3, workder id: 0
task: 4, workder id: 1
task: 5, workder id: 2
task: 6, workder id: 0
task: 7, workder id: 1
task: 8, workder id: 2
task: 9, workder id: 0
task: -1, workder id: 1
#
notes: you can use 'export CILK_NWORKERS=N' to set the max workders of cilk plus runtime.
*/

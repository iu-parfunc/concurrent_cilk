//@tests: make sure that the c++ API is working 

#include <unistd.h>
#include <stdio.h>
#include <cilk/cilk.h>
#include <cilk/ivar.h>

void fun() {
    ivar<int> iv;

    //read on the slow path
    printf("read before write -slow path\n");
    cilk_spawn iv.get();

    //write the ivar
    printf("write ivar\n");
    iv.put(39);

    //read on the fast path
    printf("   Ivar read successfully: %d\n", iv.get());
}

int main(int argc, char **argv) 
{

  cilk_spawn fun();
  cilk_sync;
  printf("test complete\n");
  return 0;
}

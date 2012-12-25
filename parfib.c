//#include "cilk/cilk.h"
#include <stdio.h>
#include <stdlib.h>

long pfib(int n) {
 if (n<2) return 1;

 long x;
 long y;
 x = pfib(n-1);
 y = cilk_spawn pfib(n-2);
 
 //this runs serially! compiler bug?
 //long x = pfib(n-1);
 //long y = cilk_spawn pfib(n-2);

 cilk_sync;
 return x+y;
}

void main(int argc, char** argv) {
 int n;
 if (argc>1)
   n = atoi(argv[1]);
 else
   n = 42;
 printf("Running pfib of %d...\n", n);
 printf("Fib(%d) is %ld\n", n, pfib(n));
}

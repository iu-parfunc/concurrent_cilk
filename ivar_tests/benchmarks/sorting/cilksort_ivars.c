/*
 * Copyright (c) 2000 Massachusetts Institute of Technology
 * Copyright (c) 2000 Matteo Frigo
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*
 * this program uses an algorithm that we call `cilksort'.
 * The algorithm is essentially mergesort:
 *
 *   cilksort(in[1..n]) =
 *       spawn cilksort(in[1..n/2], tmp[1..n/2])
 *       spawn cilksort(in[n/2..n], tmp[n/2..n])
 *       sync
 *       spawn cilkmerge(tmp[1..n/2], tmp[n/2..n], in[1..n])
 *
 *
 * The procedure cilkmerge does the following:
 *       
 *       cilkmerge(A[1..n], B[1..m], C[1..(n+m)]) =
 *          find the median of A \union B using binary
 *          search.  The binary search gives a pair
 *          (ma, mb) such that ma + mb = (n + m)/2
 *          and all elements in A[1..ma] are smaller than
 *          B[mb..m], and all the B[1..mb] are smaller
 *          than all elements in A[ma..n].
 *
 *          spawn cilkmerge(A[1..ma], B[1..mb], C[1..(n+m)/2])
 *          spawn cilkmerge(A[ma..m], B[mb..n], C[(n+m)/2 .. (n+m)])
 *          sync
 *
 * The algorithm appears for the first time (AFAIK) in S. G. Akl and
 * N. Santoro, "Optimal Parallel Merging and Sorting Without Memory
 * Conflicts", IEEE Trans. Comp., Vol. C-36 No. 11, Nov. 1987 .  The
 * paper does not express the algorithm using recursion, but the
 * idea of finding the median is there.
 *
 * For cilksort of n elements, T_1 = O(n log n) and
 * T_\infty = O(log^3 n).  There is a way to shave a
 * log factor in the critical path (left as homework).
 */

#ifdef __INTEL_COMPILER
#include <cilk/cilk.h>
// #include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>
#else
#define cilk_sync
#define cilk_spawn
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>

// #define MERGE_MEMCOPY_OPT
// #define MERGE_REMAINDER_OPT

// TEMP: Duplicating here:
#define IVAR_SHIFT 0x4
#define IVAR_READY(iv) (iv & 0xf) == 1
#define TAG(iv)   (iv << IVAR_SHIFT)
#define UNTAG(iv) (iv >> IVAR_SHIFT)
#define FULLIVAR(x) ((x<<IVAR_SHIFT) + 1)

// Ensure that this works.  We should expose this from the IVar API:
#define ALLOC_EMPTY_IVARS(size) calloc(size, sizeof(__cilkrts_ivar))
// #define ALLOC_EMPTY_IVARS(size) malloc(size * sizeof(__cilkrts_ivar))

// #define FULLIVAR(x) (x)

// typedef uint32_t ELM;
typedef uint64_t ELM;

/* MERGESIZE must be >= 2 */
#define KILO 1024
// #define KILO 32
#define MERGESIZE (2*KILO)
#define QUICKSIZE (2*KILO)
#define INSERTIONSIZE 20

static unsigned long rand_nxt = 0;

static inline unsigned long my_rand(void)
{
     rand_nxt = rand_nxt * 1103515245 + 12345;
     return rand_nxt;
}

static inline void my_srand(unsigned long seed)
{
     rand_nxt = seed;
}

static inline unsigned long long getticks()
{
     struct timeval t;
     gettimeofday(&t, 0);
     return t.tv_sec * 1000000ULL + t.tv_usec;
}

static inline double ticks_to_seconds(unsigned long long ticks)
{
     return ticks * 1.0e-6;
}

static inline ELM med3(ELM a, ELM b, ELM c)
{
     if (a < b) {
	  if (b < c) {
	       return b;
	  } else {
	       if (a < c)
		    return c;
	       else
		    return a;
	  }
     } else {
	  if (b > c) {
	       return b;
	  } else {
	       if (a > c)
		    return c;
	       else
		    return a;
	  }
     }
}

/*
 * simple approach for now; a better median-finding
 * may be preferable
 */
static inline ELM choose_pivot(ELM *low, ELM *high)
{
     return med3(*low, *high, low[(high - low) / 2]);
}

static ELM *seqpart(ELM *low, ELM *high)
{
     ELM pivot;
     ELM h, l;
     ELM *curr_low = low;
     ELM *curr_high = high;

     pivot = choose_pivot(low, high);

     while (1) {
	  while ((h = *curr_high) > pivot)
	       curr_high--;

	  while ((l = *curr_low) < pivot)
	       curr_low++;

	  if (curr_low >= curr_high)
	       break;

	  *curr_high-- = l;
	  *curr_low++ = h;
     }

     /*
      * I don't know if this is really necessary.
      * The problem is that the pivot is not always the
      * first element, and the partition may be trivial.
      * However, if the partition is trivial, then
      * *high is the largest element, whence the following
      * code.
      */
     if (curr_high < high)
	  return curr_high;
     else
	  return curr_high - 1;
}

#define swap(a, b) \
{ \
  ELM tmp;\
  tmp = a;\
  a = b;\
  b = tmp;\
}

static void insertion_sort(ELM *low, ELM *high)
{
     ELM *p, *q;
     ELM a, b;

     for (q = low + 1; q <= high; ++q) {
	  a = q[0];
	  for (p = q - 1; p >= low && (b = p[0]) > a; p--)
	       p[1] = b;
	  p[1] = a;
     }
}

/*
 * tail-recursive quicksort, almost unrecognizable :-)
 */
void seqquick(ELM *low, ELM *high)
{
     ELM *p;

     while (high - low >= INSERTIONSIZE) {
	  p = seqpart(low, high);
	  seqquick(low, p);
	  low = p + 1;
     }

     insertion_sort(low, high);
}

void seqmerge(__cilkrts_ivar *low1, __cilkrts_ivar *high1, __cilkrts_ivar *low2, __cilkrts_ivar *high2,
	      __cilkrts_ivar *lowdest)
{
     ELM a1, a2;

     /*
      * The following 'if' statement is not necessary
      * for the correctness of the algorithm, and is
      * in fact subsumed by the rest of the function.
      * However, it is a few percent faster.  Here is why.
      *
      * The merging loop below has something like
      *   if (a1 < a2) {
      *        *dest++ = a1;
      *        ++low1;
      *        if (end of array) break;
      *        a1 = *low1;
      *   }
      *
      * Now, a1 is needed immediately in the next iteration
      * and there is no way to mask the latency of the load.
      * A better approach is to load a1 *before* the end-of-array
      * check; the problem is that we may be speculatively
      * loading an element out of range.  While this is
      * probably not a problem in practice, yet I don't feel
      * comfortable with an incorrect algorithm.  Therefore,
      * I use the 'fast' loop on the array (except for the last 
      * element) and the 'slow' loop for the rest, saving both
      * performance and correctness.
      */
// [2013.03.20] This does make a 2^25 sort a bit faster:
// But it's more like ONE PERCENT:
#ifdef MERGE_REMAINDER_OPT
     if (low1 < high1 && low2 < high2) {
	  a1 = *low1;
	  a2 = *low2;
	  for (;;) {
	       if (a1 < a2) {
		    *lowdest++ = a1; // IVAR this WRITE.
		    a1 = *++low1;
		    if (low1 >= high1)
			 break;
	       } else {
		    *lowdest++ = a2; // IVAR this WRITE.
		    a2 = *++low2;
		    if (low2 >= high2)
			 break;
	       }
	  }
     }
#endif
     if (low1 <= high1 && low2 <= high2) {
       a1 = __cilkrts_ivar_read(low1);  
       a2 = __cilkrts_ivar_read(low2); 
        //  a1 = *low1; // IVAR READ 
	// a2 = *low2; // IVAR READ
	  for (;;) {
	       if (a1 < a2) {
		  __cilkrts_ivar_write(lowdest,a1); lowdest++; 
		 //*lowdest++ = a1; // IVAR this WRITE.
		    ++low1;
		    if (low1 > high1)
			 break;
		    a1 = __cilkrts_ivar_read(low1); 
		    // a1 = *low1; // IVAR READ 
	       } else {
		  __cilkrts_ivar_write(lowdest,a1); lowdest++; 
		  // *lowdest++ = a2; // IVAR this WRITE.
		    ++low2;
		    if (low2 > high2)
			 break;
		    a2 = __cilkrts_ivar_read(low2); 
		    // a2 = *low2;
	       }
	  }
     }
#ifdef MERGE_MEMCOPY_OPT
     if (low1 > high1) {       
          memcpy(lowdest, low2, sizeof(ELM) * (high2 - low2 + 1));
     } else {
	  memcpy(lowdest, low1, sizeof(ELM) * (high1 - low1 + 1));
     }
#else 
     if (low1 > high1) {       
       int i; int size=high2-low2+1;
       // for (i=0; i<size; i++) lowdest[i] = low2[i];
       for (i=0; i<size; i++) __cilkrts_ivar_write(lowdest+i, __cilkrts_ivar_read(low2+i));
     } else {
       int i; int size=high1-low1+1;
       // for (i=0; i<size; i++) lowdest[i] = low1[i];
       for (i=0; i<size; i++) __cilkrts_ivar_write(lowdest+i, __cilkrts_ivar_read(low1+i));
     }
#endif
}

#define swap_indices(a, b) \
{ \
  ELM *tmp;\
  tmp = a;\
  a = b;\
  b = tmp;\
}

__cilkrts_ivar *binsplit(ELM val, __cilkrts_ivar *low, __cilkrts_ivar *high)
{
     /*
      * returns index which contains greatest element <= val.  If val is
      * less than all elements, returns low-1
      */
     __cilkrts_ivar *mid;

     while (low != high) {
	  mid = low + ((high - low + 1) >> 1);
	  if (val <= __cilkrts_ivar_read(mid))
	       high = mid - 1;
	  else
	       low = mid;
     }

     if (__cilkrts_ivar_read(low) > val)
	  return low - 1;
     else
	  return low;
}

void cilkmerge(__cilkrts_ivar *low1, __cilkrts_ivar *high1, __cilkrts_ivar *low2,
	       __cilkrts_ivar *high2, __cilkrts_ivar *lowdest)
{
     /*
      * Cilkmerge: Merges range [low1, high1] with range [low2, high2] 
      * into the range [lowdest, ...]  
      */

     __cilkrts_ivar *split1, *split2;	/*
				 * where each of the ranges are broken for 
				 * recursive merge 
				 */
     long int lowsize;		/*
				 * total size of lower halves of two
				 * ranges - 2 
				 */

     /*
      * We want to take the middle element (indexed by split1) from the
      * larger of the two arrays.  The following code assumes that split1
      * is taken from range [low1, high1].  So if [low1, high1] is
      * actually the smaller range, we should swap it with [low2, high2] 
      */

     /* if (high2 - low2 > high1 - low1) { */
     /* 	  swap_indices(low1, low2); */
     /* 	  swap_indices(high1, high2); */
     /* } */
     if (high2 - low2 > high1 - low1) {
       cilkmerge(low2,high2,low1,high1,lowdest);
       return;
     }
// [2013.03.20] Actually: this one makes things SLOWER on 2^25 atm:
#ifdef MERGE_MEMCOPY_OPT
     if (high1 < low1) {
	  /* smaller range is empty */
	  memcpy(lowdest, low2, sizeof(ELM) * (high2 - low2));
	  return;
     }
#endif
     if (high2 - low2 < MERGESIZE) {
	  seqmerge(low1, high1, low2, high2, lowdest);
	  return;
     }
     /*
      * Basic approach: Find the middle element of one range (indexed by
      * split1). Find where this element would fit in the other range
      * (indexed by split 2). Then merge the two lower halves and the two
      * upper halves. 
      */

     split1 = ((high1 - low1 + 1) / 2) + low1;
     split2 = binsplit(*split1, low2, high2);
     lowsize = split1 - low1 + split2 - low2;

     /* 
      * directly put the splitting element into
      * the appropriate location
      */     
     /* printf("Trying to write split in... ivar at address %p, offset %ld, writing val %ld, val low bits %ld\n", */
     /* 	    lowdest + lowsize + 1, lowsize + 1, *split1, *split1 & 0xf); */
     // FIXME: Shouldn't have to untag here...
     __cilkrts_ivar_write(lowdest + lowsize + 1, UNTAG(*split1));
     // *(lowdest + lowsize + 1) = *split1; // IVAR this WRITE...
     cilk_spawn cilkmerge(low1, split1 - 1, low2, split2, lowdest);
     cilk_spawn cilkmerge(split1 + 1, high1, split2 + 1, high2,
			  lowdest + lowsize + 2);
     cilk_sync;
     return;
}

__cilkrts_ivar* cilksort(ELM *low, long size)
{
     /*
      * divide the input in four parts of the same size (A, B, C, D)
      * Then:
      *   1) recursively sort A, B, C, and D (in parallel)
      *   2) merge A and B into tmp1, and C and D into tmp2 (in parallel)
      *   3) merbe tmp1 and tmp2 into the original array
      */
     long quarter = size / 4;
     long lastquarter = size - 3 * quarter;
     ELM *A, *B, *C, *D;
     __cilkrts_ivar *tmpA, *tmpB, *tmpC, *tmpD;
     __cilkrts_ivar *result, *result2;

     if (size < QUICKSIZE) {
	  /* quicksort when less than 1024 elements */
	  seqquick(low, low + size - 1);
          // TODO: probably have to malloc and memcopy here?  To match the contract...
	  // return low;

          // Uh, using this in lieu of an out of place sort:
          result = (__cilkrts_ivar*) malloc(size * sizeof(__cilkrts_ivar));
          // DISADVANTAGE OF IVARS:  Can't just memcpy this.. Have to at least mark full bits:
	  // memcpy(result,low,size * sizeof(ELM));
	  int i; for(i=0; i<size; i++) result[i] = FULLIVAR(low[i]);
	  // seqquick(result, result + size - 1);
	  return result;
     }

     A = low;
     B = A + quarter;
     C = B + quarter;
     D = C + quarter;

     tmpA = cilk_spawn cilksort(A, quarter);
     tmpB = cilk_spawn cilksort(B, quarter);
     tmpC = cilk_spawn cilksort(C, quarter);
     tmpD = cilk_spawn cilksort(D, lastquarter);
     cilk_sync;

     //     printf("[%ld] ALL spawned sorts finished!!\n", size);

     // COPY results back into tmp:
     /* memcpy(tmp            , tmpA, sizeof(ELM) * quarter); */
     /* memcpy(tmp +   quarter, tmpB, sizeof(ELM) * quarter); */
     /* memcpy(tmp + 2*quarter, tmpC, sizeof(ELM) * quarter); */
     /* memcpy(tmp + 3*quarter, tmpC, sizeof(ELM) * lastquarter); */

     //     printf("  [%ld] Results copied back into tmp...\n",size);

     /* cilk_spawn cilkmerge(A, A + quarter - 1, B, B + quarter - 1, tmpA); */
     /* cilk_spawn cilkmerge(C, C + quarter - 1, D, low + size - 1,  tmpC); */

     result = ALLOC_EMPTY_IVARS(size);

     // ONE OPTION -- if we liked we could read all the IVar results
     // back out and put them in an array of plain results before alling
     // cilkmerge... NOT doing that for now. -RRN [2013.03.21]

     // Each of these merges writes out a half-sized chunk:
     cilk_spawn cilkmerge(tmpA, tmpA + quarter - 1, tmpB, tmpB + quarter - 1, result);
     cilk_spawn cilkmerge(tmpC, tmpC + quarter - 1, tmpD, tmpD + lastquarter - 1, result+2*quarter);

     cilk_sync;

     /* cilk_spawn cilkmerge(tmpA, tmpC - 1, tmpC, tmpA + size - 1, A); */

     // This final merge is full sized, values go to the ...
     free(tmpA); free(tmpB); free(tmpC); free(tmpD);
     result2 = ALLOC_EMPTY_IVARS(size);
     cilk_spawn cilkmerge(result, result + 2*quarter - 1, result+2*quarter, result + size - 1, result2);

     cilk_sync;
     free(result);  
     return result2;
}

void scramble_array(ELM *arr, unsigned long size)
{
     unsigned long i;
     unsigned long j;

     for (i = 0; i < size; ++i) {
	  j = my_rand();
	  j = j % size;
	  swap(arr[i], arr[j]);
     }
}

void fill_array(ELM *arr, unsigned long size)
{
     unsigned long i;

     my_srand(1);
     /* first, fill with integers 1..size */
     for (i = 0; i < size; ++i) {
	  arr[i] = i;
     }

     /* then, scramble randomly */
     scramble_array(arr, size);
}


/* creates arrays and measures cilksort() running time */
unsigned long long run_cilksort(long size)
{
     ELM *array;
     __cilkrts_ivar *result;
     unsigned long long start, end, t1;
     int success;
     long i;

     array = (ELM *) malloc(size * sizeof(ELM));
     //     tmp = (ELM *) malloc(size * sizeof(ELM));

     cilk_spawn fill_array(array, size);
     cilk_sync;

     /* Timing. "Start" timers */
     cilk_sync;
     start = getticks();

     result = cilk_spawn cilksort(array, size);
     cilk_sync;

     /* Timing. "Stop" timers */
     end = getticks();

     t1 = end - start;

     success = 1;
     for (i = 0; i < size; ++i)
       // TODO: It would be nice to have a non-blocking read here to ensure it's full:
       // FIXME: Getting problems with this:
#if 1
       if (__cilkrts_ivar_read(result + i) != i) 
#else
       if (UNTAG(result[i]) != i)
#endif
       {
	 success = 0;
	 break;
       }

     if (!success) {
          int j;
          printf("SORTING FAILURE, prefix, position %ld:\n ... ", i);
          // j = i - 2; if (j<0) j=0;
          j = i;
	  for(; j < i+8; j++) {
	    if (j<size) printf("%ld ", result[j]);
	  }
          printf("\nUntagged: \n ... "); j = i;
	  for(; j < i+8; j++) {
	    if (j<size) printf("%ld ", UNTAG(result[j]));
	  }
	  printf("\n");
     } else {
          printf("\nCilk Example: cilksort\n");
          printf("Number of elements: %ld\n", size);
          printf("Executed in: %llu ticks (%f s)\n\n",
                 t1, ticks_to_seconds(t1));
     }

     free(array);
     free(result); 

     return t1;
}

int usage(void)
{
     fprintf(stderr, "\nUsage: cilksort [<cilk-options>] [-n size] [-b benchmark] [-h]\n\n");
     fprintf(stderr, "Cilksort is a parallel sorting algorithm, donned \"Multisort\", which\n");
     fprintf(stderr, "is a variant of ordinary mergesort.  Multisort begins by dividing an\n");
     fprintf(stderr, "array of elements in half and sorting each half.  It then merges the\n");
     fprintf(stderr, "two sorted halves back together, but in a divide-and-conquer approach\n");
     fprintf(stderr, "rather than the usual serial merge.\n\n");

     return -1;
}

extern char *optarg;

// #if (USE_MAIN==1)
int main(int argc, char **argv)
{
     long size;
     int c, benchmark, help;

     /* standard benchmark options */
     size = 400;
     help = 0;
     benchmark = 0;

#if 0
     while ((c = getopt (argc, argv, "hb:n:")) != -1) {
	 switch (c) {
	 case 'h':
	     return usage();
	 case 'b':
	     benchmark = strtol(optarg, NULL, 10);
	     printf ("benchmark: %d\n", benchmark);
	     break;
	 case 'n':
	     size = strtol(optarg, NULL, 10);
	     break;
	 default:
	     break;
	 }
     }
#else
     //     benchmark = 3;
#endif

     if (argc > 1) {
       size = strtol(argv[1], NULL, 10);
       printf("Setting size based on command line arg: %ld\n", size);
     }

     if (benchmark) {
	 switch (benchmark) {
	 case 1:		/* short benchmark options -- a little work */
	     size = 10000;
	     break;
	 case 2:		/* standard benchmark options */
	     size = 3000000;
	     break;
	 case 3:		/* long benchmark options -- a lot of work */
	     size =  4100000; // About 16M .. i guess this was supposed to fit in cache.
	    // size = 10000000; // RRN: pumping this up.  10M e.g. 40Mb
	    // size = 1 << 24; 
	     break;
	 }
     }

     run_cilksort(size);
     
     return 0;
}
// #endif

// HOWTO: build with GCC-4.7/Cilk:
// $ gcc -lm -lcilkrts cilksort.c -o cilksort_gcc.exe

// Or you can use ICC.


#ifndef __CONCURRENT_CILK_INTERNAL_H
#define __CONCURRENT_CILK_INTERNAL_H


#include "os.h"
#include "cilk/concurrent_cilk.h"
#include "full_frame.h"

#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)
#define BEGIN_WITH_FRAME_LOCK(w, ff)                                     \
    do { full_frame *_locked_ff = ff; __cilkrts_frame_lock(w, _locked_ff); do

#define END_WITH_FRAME_LOCK(w, ff)                       \
    while (__cilkrts_frame_unlock(w, _locked_ff), 0); } while (0)
// This can be any constant that is not in the range of addresses returned by malloc:
// it is used as the flag value to check if an ivar is full or empty
__CILKRTS_BEGIN_EXTERN_C


/** hedge our bets on an if statement 
 * if we have a pretty good idea of what the result will be 
 */
#define if_t(test) if (__builtin_expect(test,1)) 
#define if_f(test) if (__builtin_expect(test,0)) 

/**
 * syntactic sugar
 */
#define spin_pause() __asm__("pause")
#define cas(ptr,oldval,newval) __sync_bool_compare_and_swap(ptr,oldval,newval)
#define casv(ptr,oldval,newval) __sync_val_compare_and_swap(ptr,oldval,newval)
#define atomic_add(ptr,num) __sync_fetch_and_add(ptr,num)
#define atomic_sub(ptr,num) __sync_fetch_and_sub(ptr,num)
#define atomic_set(ptr,val) __sync_lock_test_and_set(ptr,val)
#define atomic_release(ptr, val) __sync_lock_release(ptr,val)
#define align(n) __attribute__((aligned(n)))
//#define clear_cache(begin,end) __builtin___clear_cache(begin,end);

/**
 * read only/read write levels for prefetch 
 */
#define READ_WRITE 1
#define READ_ONLY  0

/**
 * cache locality levels
 */
#define NONE 0
#define LOW 1
#define MED 2
#define HIGH 3

/**
 * prefetch if at all possible
 */
#define prefetch(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define prefetch_rw(addr,locality) __builtin_prefetch(addr,READ_WRITE,locality)
#define prefetch_r(addr,locality)  __builtin_prefetch(addr,READ_ONLY,locality)


/* struct tags */
typedef struct __cilkrts_worker      __cilkrts_worker;
typedef struct __cilkrts_paused_fiber __cilkrts_paused_fiber;
typedef struct queue_t queue_t;

/*   Concurrent Cilk:  Types & API   */

 typedef struct __cilkrts_paused_fiber {

   __cilkrts_worker *paused;
   __cilkrts_worker *replacement;

  /* Paused stack internal record keeping */
  //-----------------------------
  __cilkrts_paused_fiber *head;

  __cilkrts_paused_fiber *tail;

  __cilkrts_paused_fiber *next;

  jmp_buf ctx; //TODO: can use cilk jmp_buf?

  int lock; 
  //-----------------------------

} align(64) __cilkrts_paused_fiber;

/* Lock API for paused stacks */
int paused_fiber_trylock(__cilkrts_paused_fiber *pfiber);
void paused_fiber_unlock(__cilkrts_paused_fiber *pfiber);

/* Scheduler functions */
__cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim);

__CILKRTS_END_EXTERN_C
#endif


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

#define IVAR 0x1
#define CONCURRENT 0x2
#define FRAME 0x4

#ifdef CILK_IVARS
#ifdef IVAR_DBG

#define dbgprint(lvl, ...) if (lvl & IVAR_DBG) {\
  pthread_t   tid;\
  tid = pthread_self();\
  fprintf(stderr, "[tid:%4d] ", (int)(((int)tid)%10000)); fflush(stderr);\
  fprintf(stderr, __VA_ARGS__); fflush(stderr); \
}

#else

#define dbgprint(lvl, ...)
#endif
#endif

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

#define waitlock_atomic(lock) \
  while(!__sync_bool_compare_and_swap(lock, 0, 1)) __asm__("pause")
#define trylock_atomic(lock) __sync_bool_compare_and_swap(lock, 0, 1)
#define unlock_atomic(lock)  __sync_bool_compare_and_swap(lock, 1, 0)

#define MAX_WORKERS_BLOCKED 16384


/*   Concurrent Cilk:  Types & API   */

/* struct tags */
typedef struct __cilkrts_worker      __cilkrts_worker;


/* Scheduler functions */
__cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim);
__cilkrts_worker *find_ready_fiber(__cilkrts_worker *victim);

inline void remove_worker_from_stealing(__cilkrts_worker *w);
void register_worker_for_stealing(__cilkrts_worker *w); 
void __cilkrts_push_concurrent_sync_frame(__cilkrts_worker *w, full_frame *ff);
full_frame *pop_concurrent_sync_frame(__cilkrts_worker *w);

__CILKRTS_END_EXTERN_C
#endif


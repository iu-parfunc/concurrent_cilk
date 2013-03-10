#ifndef __CONCURRENT_CILK_INTERNAL_H
#define __CONCURRENT_CILK_INTERNAL_H


#include "cilk/concurrent_cilk.h"
#include "concurrent_cilk_forwarding_array.h"
#include "full_frame.h"
// This can be any constant that is not in the range of addresses returned by malloc:
// it is used as the flag value to check if an ivar is full or empty
__CILKRTS_BEGIN_EXTERN_C

#define CILK_IVAR_FULL 1

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
#define atomic_add(ptr,num) __sync_fetch_and_add(ptr,num)
#define atomic_sub(ptr,num) __sync_fetch_and_sub(ptr,num)
#define atomic_set(ptr,val) __sync_lock_test_and_set(ptr,val)
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
typedef struct __cilkrts_worker*     __cilkrts_worker_ptr;
typedef struct __cilkrts_stack_frame __cilkrts_stack_frame;
typedef struct __cilkrts_forwarding_array __cilkrts_forwarding_array;
typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;
typedef struct queue_t queue_t;
typedef struct __cilkrts_stack_pair __cilkrts_stack_pair;
typedef struct __cilkrts_ivar_waitlist __cilkrts_ivar_waitlist;

// Forwarded declarations
typedef struct global_state_t        global_state_t;
typedef struct local_state           local_state;
typedef struct cilkred_map           cilkred_map;
typedef struct __cilkrts_worker_sysdep_state __cilkrts_worker_sysdep_state;


/*   Concurrent Cilk:  Types & API   */

 struct __cilkrts_paused_stack {
    struct __cilkrts_stack* stack;

    /// For this variant the new worker does not cache the *stalled* workers state, instead 
    /// the stalled worker stays put and we have a fresh replacement worker:
    __cilkrts_worker* replacement_worker;

    /// And this is the original worker that got stalled, in its original location:
    __cilkrts_worker* orig_worker;  // Should be NON-NULL

#if CILK_IVARS == CILK_IVARS_PTHREAD_VARIANT
    /// This is a private condition used by only this worker/stack.
    pthread_cond_t cond;
    pthread_mutex_t mut;
#endif
} __attribute__((aligned(64)));

/*   Cilk IVars:  Types & API   */

__cilkrts_worker* replace_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker, volatile __cilkrts_paused_stack* stk);
__cilkrts_paused_stack* make_paused_stack(__cilkrts_worker* w);
int paused_stack_lock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk);
int paused_stack_unlock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk);
void __cilkrts_concurrent_yield(__cilkrts_worker *w);
void my_resume (__cilkrts_worker *w, full_frame *f, __cilkrts_stack_frame *sf);
void restore_worker2(__cilkrts_worker* old_w, volatile  __cilkrts_paused_stack* stk);
void setup_and_invoke_scheduler(__cilkrts_worker *w);

__CILKRTS_END_EXTERN_C
#endif


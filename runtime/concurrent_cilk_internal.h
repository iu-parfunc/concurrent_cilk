#ifndef __CONCURRENT_CILK_INTERNAL_H
#define __CONCURRENT_CILK_INTERNAL_H


#include "cilk/concurrent_cilk.h"
#include "full_frame.h"

#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)

#define BEGIN_WITH_FRAME_LOCK(w, ff) \
    do { full_frame *_locked_ff = ff; __cilkrts_frame_lock(w, _locked_ff); do

#define END_WITH_FRAME_LOCK(w, ff) \
    while (__cilkrts_frame_unlock(w, _locked_ff), 0); } while (0)

#define BEGIN_WITH_WORKER_AND_FRAME_LOCK(w, ff) \
  do { \
    __cilkrts_worker_lock(w); \
    full_frame *_locked_ff = ff; \
    __cilkrts_frame_lock(w, _locked_ff); do  

#define END_WITH_WORKER_AND_FRAME_LOCK(w, ff) \
    while (__cilkrts_frame_unlock(w, _locked_ff), 0); } \
while (__cilkrts_worker_unlock(w), 0)

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
#define hotspot __attribute__ ((hot))
#define coldspot __attribute__ ((cold))
#define pure_func __attribute__ ((pure))
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

typedef struct __cilkrts_paused_stack {

  /* Saved state fields */
  //-----------------------------
  __cilkrts_worker *w;

  full_frame *ff;

  __cilkrts_worker *team;

  int flags;

  full_frame *paused_ff;

  void *scheduler_stack;

  __cilkrts_stack_frame *current_stack_frame;

  jmp_buf env;
  //-----------------------------

  /* Paused stack internal record keeping */
  //-----------------------------
  __cilkrts_paused_stack *head;

  __cilkrts_paused_stack *tail;

  __cilkrts_paused_stack *next;

  jmp_buf ctx;

  volatile int lock; 
  //-----------------------------

} __cilkrts_paused_stack;

/* Lock API for paused stacks */
int paused_stack_trylock(__cilkrts_paused_stack *pstk);
void paused_stack_unlock(__cilkrts_paused_stack *pstk);

/*   Cilk IVars:  Types & API   */
void __cilkrts_concurrent_yield(__cilkrts_worker *w);
void thaw_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk);
void freeze_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk);

/** callback executed when a self steal returns. --currently a no op */
void do_return_from_self (__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf);

/** restore all paused computations from the ready queue of the worker*/
void restore_ready_computations(__cilkrts_worker *w);

/** the hook for concurrent cilk into leave frame in cilk-abi.c */
void __concurrent_cilk_leave_frame_hook(__cilkrts_worker *w, __cilkrts_stack_frame *sf);

/** calls the __cilkrts_scheduler. This creates a new scheduler stack currently. */
void __setup_replacement_stack_and_run(__cilkrts_worker *w);

/** the slow path for an ivar read. the fast path is inlined in ivar_read */
ivar_payload_t slow_path(__cilkrts_ivar *ivar);

/** special scheduler for ivars */
void __concurrent_cilk_sched(__cilkrts_worker *w);

/** steal from the current worker's queue */
void self_steal(__cilkrts_worker *w);

/** restore a blocked computation */
void restore_ready_computation(__cilkrts_worker *w);

/** steal the ready_queue from the victim and assign it to the thief's restore queue if possible */
int steal_queue(__cilkrts_worker *thief, __cilkrts_worker *victim);

int setup_restore_queue(__cilkrts_worker *w, __cilkrts_worker *victim);
queue_t *replace_queue(__cilkrts_worker *w);
void do_restoration(__cilkrts_worker *w);

__CILKRTS_END_EXTERN_C
#endif


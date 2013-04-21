#ifndef __CONCURRENT_CILK_INTERNAL_H
#define __CONCURRENT_CILK_INTERNAL_H


#include "cilk/concurrent_cilk.h"
#include "full_frame.h"
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

   __cilkrts_worker *w;

  void *scheduler_stack;

  __cilkrts_worker *team;

  short is_blocked;

  jmp_buf env;

  jmp_buf ctx;

  full_frame *ff;

  __cilkrts_stack_frame *current_stack_frame;

  __cilkrts_paused_stack *waitlist;

} __cilkrts_paused_stack;

/*   Cilk IVars:  Types & API   */

//__cilkrts_paused_stack* make_paused_stack(__cilkrts_worker* w);
//inline int make_paused_stack(__cilkrts_worker* w, __cilkrts_ivar *ivar);
int paused_stack_lock(__cilkrts_worker *w, __cilkrts_paused_stack* stk);
int paused_stack_unlock(__cilkrts_worker *w, __cilkrts_paused_stack* stk);
void __cilkrts_concurrent_yield(__cilkrts_worker *w);
__cilkrts_worker *pick_random_victim(__cilkrts_worker *w);
void restore_paused_worker(__cilkrts_paused_stack *pstk);


void do_leave_paused_frame_child(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf);
void do_return_from_self_steal(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf);

__CILKRTS_END_EXTERN_C
#endif


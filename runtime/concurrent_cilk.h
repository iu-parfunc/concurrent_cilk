#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>
#include <internal/abi.h>
#include "worker_mutex.h"
#include "global_state.h"
#include <pthread.h>


__CILKRTS_BEGIN_EXTERN_C

// This can be any constant that is not in the range of addresses returned by malloc:
// it is used as the flag value to check if an ivar is full or empty
#define CILK_IVAR_FULL 1

/* struct tags */
typedef struct __cilkrts_worker      __cilkrts_worker;
typedef struct __cilkrts_worker*     __cilkrts_worker_ptr;
typedef struct __cilkrts_stack_frame __cilkrts_stack_frame;
typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;
typedef struct __cilkrts_stack_queue_struct __cilkrts_stack_queue;
typedef struct __cilkrts_stack_pair __cilkrts_stack_pair;

// Forwarded declarations
typedef struct global_state_t        global_state_t;
typedef struct local_state           local_state;
typedef struct cilkred_map           cilkred_map;
typedef struct __cilkrts_worker_sysdep_state __cilkrts_worker_sysdep_state;

__cilkrts_worker* replace_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker, volatile __cilkrts_paused_stack* stk);
int paused_stack_lock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk);
int paused_stack_unlock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk);
void __cilkrts_concurrent_yield(__cilkrts_worker *w);
void my_resume (__cilkrts_worker *w, full_frame *f, __cilkrts_stack_frame *sf);
void restore_worker2(__cilkrts_worker* old_w, volatile  __cilkrts_paused_stack* stk);
void __cilkrts_wake_stack(volatile __cilkrts_paused_stack* stk);
NORETURN setup_and_invoke_scheduler(__cilkrts_worker *w);
NORETURN __cilkrts_finalize_pause(__cilkrts_worker* w,  volatile __cilkrts_paused_stack* stk);
void __cilkrts_undo_pause(__cilkrts_worker* w, volatile __cilkrts_paused_stack* stk);

/*   Concurrent Cilk:  Types & API   */
 struct __cilkrts_paused_stack {
    struct __cilkrts_stack* stack;

    // For this variant the new worker does not cache the *stalled* workers state, instead 
    // the stalled worker stays put and we have a fresh replacement worker:
    volatile struct __cilkrts_worker* replacement_worker;

    // And this is the original worker that got stalled, in its original location:
    volatile struct __cilkrts_worker* orig_worker;  // Should be NON-NULL

    // 0/1 Flag set to indicate that the work is ready to resume.
    // Whoever sets the flag (atomically) to 1 is responsible for enqueing the task in the
    // ready queue.
    volatile int ready;    
    volatile int lock_inform;
    volatile int lock_success;

#if CILK_IVARS == CILK_IVARS_PTHREAD_VARIANT
    // This is a private condition used by only this worker/stack.
    pthread_cond_t cond;
    pthread_mutex_t mut;
#endif
};

/*   Cilk IVars:  Types & API   */
struct __cilkrts_ivar_waitlist {
    struct __cilkrts_paused_stack* stalled;
    struct __cilkrts_ivar_waitlist* tail; // null to terminate.
};


__CILKRTS_END_EXTERN_C
#endif


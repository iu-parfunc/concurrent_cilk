// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.

// API & Usage notes:
// 
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.
//
#include <stdio.h>
#include "scheduler.h"
#include <time.h>
#include "concurrent_cilk_internal.h"
#include "bug.h"
#include "os.h"
#include "os_mutex.h"
#include "local_state.h"
#include "signal_node.h"
#include "full_frame.h"
#include "stacks.h"
#include "sysdep.h"
#include "except.h"
#include "cilk_malloc.h"
#include "pedigrees.h"
#include <cilk/concurrent_queue.h>
#include <setjmp.h>

//for nanosleep "declared implicity"...it should be in time.h but isn't?
#pragma warning(disable: 266)

//for atomic_release
#pragma warning(disable: 2206)

#define VALIDATE_NOT_FLAG_CILK_FRAME_BLOCKED(w, sf) \
  if (__builtin_expect(sf->flags & CILK_FRAME_BLOCKED, 0)) { \
     __cilkrts_bug("W%u: tried to run a blocked frame. Function exiting with invalid cilk frame flags %x.\n", \
        w->self, sf->flags); \
  }

#define VALIDATE_NOT_FLAG_WORKER_RESUMING(w) \
  if(__builtin_expect(w->concurrent_worker_state & CILK_WORKER_RESTORING, 0)) { \
     __cilkrts_bug("W%u: tried to block a worker while either restoring. " \
         "Function exiting with invalid worker flags %x.\n", \
        w->self, w->concurrent_worker_state); \
  }

void print_sf_flags(__cilkrts_stack_frame *sf) 
{
  printf("<<<< flags returning: ");
  if(sf->flags & CILK_FRAME_BLOCKED_RETURNING) printf("CILK_FRAME_BLOCKED_RETURNING ");
  if(sf->flags & CILK_FRAME_BLOCKED)           printf("CILK_FRAME_BLOCKED ");
  if(sf->flags & CILK_FRAME_SELF_STEAL)        printf("CILK_FRAME_SELF_STEAL ");
  if(sf->flags & CILK_FRAME_STOLEN)            printf("CILK_FRAME_STOLEN ");
  if(sf->flags & CILK_FRAME_UNSYNCHED)         printf("CILK_FRAME_UNSYNCHED ");
  if(sf->flags & CILK_FRAME_DETACHED)          printf("CILK_FRAME_DETACHED ");
  if(sf->flags & CILK_FRAME_EXCEPTING)         printf("CILK_FRAME_EXCEPTING ");
  if(sf->flags & CILK_FRAME_LAST)              printf("CILK_FRAME_LAST ");
  if(sf->flags & CILK_FRAME_EXITING)           printf("CILK_FRAME_EXITING ");
  if(sf->flags & CILK_FRAME_SUSPENDED)         printf("CILK_FRAME_SUSPENDED ");
  if(sf->flags & CILK_FRAME_UNWINDING)         printf("CILK_FRAME_UNWINDING ");
  printf("\n");
}


CILK_API(void) __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

CILK_API(void) __cilkrts_usleep(unsigned long micros)
{
  struct timespec time;
  time.tv_sec = micros/1000000ul;
  time.tv_nsec = (micros % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

inline CILK_API(void)
__cilkrts_ivar_clear(__cilkrts_ivar* ivar)
{
  *ivar = 0;
}

NOINLINE
static void worker_replacement_scheduler()
{
    __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();
    int stolen = 0;

    // Enter the scheduling loop on the worker. This function will
    // never return
    CILK_ASSERT(w->current_stack_frame);

    __cilkrts_run_scheduler_with_exceptions((__cilkrts_worker *) w);

    CILK_ASSERT(0);
}

//note: the w passed in here must be the current tls worker!
static inline void 
setup_replacement_stack_and_run(__cilkrts_worker *w)
{
    void *ctx[5]; // Jump buffer for __builtin_setjmp/longjmp.

    //TODO: cache these suckers and get from cache here
    if(! w->l->scheduler_stack)
      w->l->scheduler_stack = sysdep_make_tiny_stack(w);
    CILK_ASSERT(w->l->scheduler_stack);

    // Move the stack pointer onto the scheduler stack.  The subsequent
    // call will move execution onto that stack.  We never return from
    // that call, and every time we longjmp_into_runtime() after this,
    // the w->l->env jump buffer will be populated.
    if (0 == __builtin_setjmp(ctx)) {
        ctx[2] = w->l->scheduler_stack; // replace the stack pointer.
        __builtin_longjmp(ctx, 1);
    } else {
        // We can't just pass the worker through as a parameter to
        // worker_user_scheduler because the generated code might try to
        // retrieve w using stack-relative addressing instead of bp-relative
        // addressing and would get a bogus value.
        worker_replacement_scheduler(); // noinline, does not return.
        CILK_ASSERT(0); // Should never reach this point.
    }
}

inline void
freeze_frame(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf)
{

  //ASSERT: frame lock owned
  
  CILK_ASSERT(ff);
  CILK_ASSERT(sf);

  if(w->concurrent_worker_state & CILK_WORKER_UNBLOCKED) {
    printf("recorded last blocked full frame as %p\n", ff);
    ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED_LAST;
    w->paused_ff  = ff;
  }

  //It is not legal to block while internally restoring
  //an unblocked context. This flag being set would mean
  //that either a) we executed user code while restoring runtime state
  //on this worker (bad) or we called an ivar internally in cilk runtime (very bad). 
  VALIDATE_NOT_FLAG_WORKER_RESUMING(w);

  //worker is now marked as blocked
  w->concurrent_worker_state |= CILK_WORKER_BLOCKED;

 //setup the flags on the cilk stack frame as blocked and suspended
 //the suspended is to maintain the contract with normal cilk. The
 //blocked flag is for concurrent record keeping.
  sf->flags |= CILK_FRAME_SUSPENDED | CILK_FRAME_BLOCKED;

  //this full frame is now marked as a blocked frame
  ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED;

  /* CALL_STACK becomes valid again */
  ff->call_stack = sf;

  __cilkrts_put_stack(ff, sf);

}

//restore the context of a paused worker
  inline void
thaw_frame(__cilkrts_paused_stack *pstk) 
{

  __cilkrts_stack_frame *sf;
  full_frame *ff;

  CILK_ASSERT(pstk);
  ff = pstk->ff;
  sf = pstk->current_stack_frame;
  __cilkrts_worker *w = pstk->w;
  CILK_ASSERT(w);

  //------------restore context------------
  w->l->team                    = pstk->team;
  w->l->frame_ff                = pstk->ff;
  w->concurrent_worker_state    = pstk->concurrent_worker_state;
  w->l->scheduler_stack         = pstk->scheduler_stack;
  w->current_stack_frame        = pstk->current_stack_frame;
  w->current_stack_frame->flags = pstk->flags;
  memcpy(&w->l->env, &pstk->env, sizeof(jmp_buf));
  //------------end restore context------------

  //fixup the flags:
  //1. The stack frame loses its blocked marking
  //and is marked as a blocked frame that is now returning.
  //
  //2. The full frame loses its blocked marking and is marked as
  //being self stolen so we know to take the fast path in leave frame.
  //
  //3. The worker state gains the restoring flag. The worker should be marked
  //as both blocked and restoring at the end of this function.
  
  //---------------fixup flags--------------
  CILK_ASSERT(!(sf->flags & CILK_FRAME_BLOCKED_RETURNING));
  CILK_ASSERT(!(w->concurrent_worker_state & CILK_WORKER_UNBLOCKED));
  CILK_ASSERT(ff->concurrent_cilk_flags & FULL_FRAME_BLOCKED);

  sf->flags &= ~CILK_FRAME_BLOCKED;
  sf->flags |=  CILK_FRAME_BLOCKED_RETURNING;

  ff->concurrent_cilk_flags &= ~FULL_FRAME_BLOCKED;
  ff->concurrent_cilk_flags |= FULL_FRAME_SELF_STEAL;

  w->concurrent_worker_state |= CILK_WORKER_RESTORING;
  //---------------end fixup flags--------------

  
  //TODO cache these suckers
  //sysdep_destroy_tiny_stack(w->l->scheduler_stack);
  longjmp(pstk->ctx, 1);
  CILK_ASSERT(0); //does not return
}

// Commit the pause.
  CILK_API(void)
__cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack *pstk) 
{
  //-------------save context of the worker-------
  pstk->w                       = w;
  pstk->ff                      = w->l->frame_ff;
  pstk->team                    = w->l->team;
  pstk->flags                   = w->current_stack_frame->flags;
  //pstk->paused_ff             = w->paused_ff;
  pstk->scheduler_stack         = w->l->scheduler_stack;
  pstk->current_stack_frame     = w->current_stack_frame;
  pstk->concurrent_worker_state = w->concurrent_worker_state;
  memcpy(&pstk->env, &w->l->env, sizeof(jmp_buf));
  //---------end save context of the worker-------

  BEGIN_WITH_WORKER_LOCK(w) {

    BEGIN_WITH_FRAME_LOCK(w, w->l->frame_ff) {
      freeze_frame(w, w->l->frame_ff, w->current_stack_frame);
      w->l->frame_ff = NULL;
    } END_WITH_FRAME_LOCK(w, w->l->frame_ff);

  } END_WITH_WORKER_LOCK(w);

  setup_replacement_stack_and_run(w);
  CILK_ASSERT(0); //no return
}

int paused_stack_trylock(__cilkrts_paused_stack *pstk) {
  return cas(&pstk->lock, 0, 1);
}

void paused_stack_unlock(__cilkrts_paused_stack *pstk) {
  atomic_release(&pstk->lock, 0);
}

void restore_ready_computations(__cilkrts_worker *w) 
{
  printf("restoring ready computation\n");
  __cilkrts_paused_stack *pstk = NULL;
  //whenever a non blocked frame returns, we check to see if it can perform a
  //pop on some blocked work. The preference is to clear the worker's blocked
  //queue as we are going to shred the cache, so we might as do all the disrputive
  //operations at one shot. 
  dequeue(w->ready_queue, (ELEMENT_TYPE *) &pstk);

  if(! setjmp(w->unblocked_ctx)) {
    thaw_frame(pstk);
    CILK_ASSERT(0);
  } else {
    //segfault if the longjmp gets called twice
    memset(&w->unblocked_ctx, 0, sizeof(jmp_buf));
  }
}

// Used in cilk-abi.c in return_frame
//-------------------------------------
  void
__concurrent_cilk_leave_frame_hook(__cilkrts_worker *w, __cilkrts_stack_frame *sf)
{
  __cilkrts_paused_stack *pstk;
  __cilkrts_paused_stack *escape;
  uintptr_t ptr;

  //there should never be a blocked frame returning...ever.
  VALIDATE_NOT_FLAG_CILK_FRAME_BLOCKED(w, sf);

  /** if the frame is marked as a blocked frame now returning,
   * we shouldn't do an actual return. instead, we should jump to the escape 
   * point that was saved before the cycle of popping all ready ivars of the
   * queue was begun. After the longjmp, normal cilk operation resumes.
   */

  //whenever a non blocked frame returns, we check to see if it can perform a
  //pop on some blocked work. The preference is to clear the worker's blocked
  //queue.
  dequeue(w->ready_queue, (ELEMENT_TYPE *) &pstk);

  if (pstk != NULL) {
    if (!sf->flags & CILK_FRAME_BLOCKED_RETURNING) {
      /* If the flags are not set to blocked returning,
       * this is not a cycle of popping blocked frames.
       * We save the context here to escape the cycle
       * when we have finally finished popping
       * all the paused contexts. 
       */

      escape = __cilkrts_malloc(sizeof(__cilkrts_paused_stack));
      memset(escape, 0, sizeof(__cilkrts_paused_stack));
      ptr = (uintptr_t) __cilkrts_pause((escape->ctx));

      if (!ptr) {
        enqueue(w->ready_queue, (ELEMENT_TYPE) escape);
        thaw_frame(pstk);
        CILK_ASSERT(0);
      } else {
        printf("escaping context!\n");
      }
    }
  }

  return;
}


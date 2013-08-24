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
#include "cilk_util.h"

#include <cilk/concurrent_queue.h>
#include <setjmp.h>
#include <cilk/concurrent_cilk.h>

//for nanosleep "declared implicity"...it should be in time.h but isn't?
#pragma warning(disable: 266)

//for atomic_release
#pragma warning(disable: 2206)


void print_sf_flags(__cilkrts_stack_frame *sf) 
{
  printf("<<<< flags returning: ");
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

  __cilkrts_run_scheduler_with_exceptions(&random_work_steal_sched, (__cilkrts_worker *) w, NULL);

  CILK_ASSERT(0);
}

//note: the w passed in here must be the current tls worker!
  void 
__setup_replacement_stack_and_run(__cilkrts_worker *w)
{
  void *ctx[5]; // Jump buffer for __builtin_setjmp/longjmp.

  //TODO: cache these suckers and get from cache here
  //if(! w->l->scheduler_stack)
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
    IVAR_DBG_PRINT(1,"recorded last blocked full frame as %p\n", ff);
    ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED_LAST;
    w->paused_ff  = ff;
  }

  //worker is now marked as blocked
  w->concurrent_worker_state &= ~CILK_WORKER_UNBLOCKED;
  w->concurrent_worker_state |=  CILK_WORKER_BLOCKED;
  IVAR_DBG_PRINT(1,"%d/%p w->concurrent_worker_state now set as blocked.\n", w->self, w);

  //setup the flags on the cilk stack frame as blocked and suspended
  //the suspended is to maintain the contract with normal cilk. The
  //blocked flag is for concurrent record keeping.
  sf->flags |= CILK_FRAME_SUSPENDED;

  //this full frame is now marked as a blocked frame
  ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED;

  /* CALL_STACK becomes valid again */
  ff->call_stack = sf;

  __cilkrts_put_stack(ff, sf);

}

/**
 * Restore the context of a paused worker. 
 * In addition to the paused context, the function takes a wkr_flags
 * argument. This specifies the desired state of the worker as it
 * longjmps back into the unpaused state. The desired re-entry state
 * of the worker may need to change depending on the transition that
 * is desired. The current use case is the transition from a restoring
 * state, to an unblocked state.
 */
  inline void
thaw_frame(__cilkrts_paused_stack *pstk, uint32_t wkr_flags) 
{
  IVAR_DBG_PRINT(1,"thawing pstk: %p\n", pstk);

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
  //w->l->scheduler_stack         = pstk->scheduler_stack;
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
  //3. The worker state gains the restoring. The worker should be marked
  //as both blocked and restoring at the end of this function.

  //---------------fixup flags--------------
  //  CILK_ASSERT(ff->concurrent_cilk_flags & FULL_FRAME_BLOCKED);
  //if the worker state is entering a resume cycle for the first time,
  //it will be set to BLOCKED if it is in the middle of a resume cycle
  //it will be set to RESUMING. Any other flag on the worker is invalid.
  CILK_ASSERT((w->concurrent_worker_state & CILK_WORKER_BLOCKED) ||
      (w->concurrent_worker_state & CILK_WORKER_RESTORING));

  ff->concurrent_cilk_flags &= ~FULL_FRAME_BLOCKED;
  ff->concurrent_cilk_flags |= FULL_FRAME_SELF_STEAL;

  w->concurrent_worker_state = wkr_flags;
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
  //pstk->scheduler_stack         = w->l->scheduler_stack;
  //pstk->concurrent_worker_state = w->concurrent_worker_state;
  pstk->current_stack_frame     = w->current_stack_frame;
  memcpy(&pstk->env, &w->l->env, sizeof(jmp_buf));
  //---------end save context of the worker-------

  BEGIN_WITH_WORKER_LOCK(w) {

    BEGIN_WITH_FRAME_LOCK(w, w->l->frame_ff) {
      freeze_frame(w, w->l->frame_ff, w->current_stack_frame);
      w->l->frame_ff = NULL;
    } END_WITH_FRAME_LOCK(w, w->l->frame_ff);

  } END_WITH_WORKER_LOCK(w);

}

int paused_stack_trylock(__cilkrts_paused_stack *pstk) {
  return cas(&pstk->lock, 0, 1);
}

void paused_stack_unlock(__cilkrts_paused_stack *pstk) {
  atomic_release(&pstk->lock, 0);
}

void restore_ready_computations(__cilkrts_worker *w) 
{
  __cilkrts_paused_stack *pstk;
  __cilkrts_paused_stack escape = {0};
  uintptr_t ptr;

  pstk = NULL;
  dequeue(w->ready_queue, (ELEMENT_TYPE *) &pstk);

  if (pstk != NULL) {
    IVAR_DBG_PRINT(1,"restoring ready computation\n");
    if ((w->concurrent_worker_state  & CILK_WORKER_BLOCKED) &&
        !(w->concurrent_worker_state & CILK_WORKER_RESTORING)) {
      IVAR_DBG_PRINT(1,"creating NEW placeholder stack\n");
      /* This is the first entry into a restoring cycle to
       * clear the ivar deque. As such, we pause the current
       * return from user code and go to finish off any other
       * paused computation before returning to this one. 
       */

      ptr = (uintptr_t) __cilkrts_pause((escape.ctx));
      if (!ptr) {
        IVAR_DBG_PRINT(1,"enqueueing escape context: %p\n", &escape);
        CILK_ASSERT(w->l->frame_ff);
        __cilkrts_finalize_pause(w, &escape); 
        enqueue(w->ready_queue, (ELEMENT_TYPE) &escape);
      } else {
        IVAR_DBG_PRINT(1,"escaping context!\n");
        //The worker has its paused dequeue cleared.
        //It can now resume normal work.
        w->concurrent_worker_state &= ~CILK_WORKER_RESTORING;
        return;
      }

    } else {
      /* The only valid flag other than BLOCKED is RESTORING
       * if there is a paused stack to restore. 
       */
      IVAR_DBG_PRINT(1,"state: 0x%x\n", w->concurrent_worker_state);
      CILK_ASSERT(w->concurrent_worker_state & CILK_WORKER_RESTORING);
    }

    thaw_frame(pstk, w->concurrent_worker_state | CILK_WORKER_RESTORING);
    CILK_ASSERT(0);
  }
}

// Used in cilk-abi.c in return_frame
//-------------------------------------
  void
__concurrent_cilk_leave_frame_hook(__cilkrts_worker *w, __cilkrts_stack_frame *sf)
{
  //whenever a non blocked frame returns, we check to see if it can perform a
  //pop on some blocked work. The preference is to clear the worker's blocked
  //queue.
  restore_ready_computations(w);

  return;
}


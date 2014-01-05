// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.

// API & Usage notes:
// 
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.
//
#include <setjmp.h>
#include "scheduler.h"
#include "concurrent_cilk_internal.h"
#include <cilk/concurrent_queue.h>
#include <cilk/concurrent_cilk.h>
#include "concurrent_cilk_internal.h"

#ifdef __INTEL_COMPILER__
// for atomic_release
#  pragma warning(disable: 2206)
// for nanosleep not defined
#  pragma warning(disable: 266)
#endif




  inline CILK_API(void)
__cilkrts_ivar_clear(__cilkrts_ivar* ivar) { CILK_ASSERT(ivar); *ivar = 0; }

  inline static void 
thaw_worker_state(__cilkrts_worker *w, __cilkrts_paused_stack *pstk)
{
  CILK_ASSERT(w);
  CILK_ASSERT(pstk);

  w->l->team                    = pstk->team;
  w->l->frame_ff                = pstk->ff;
  w->current_stack_frame        = pstk->current_stack_frame;
  w->current_stack_frame->flags = pstk->flags;
  memcpy(&w->l->env, &pstk->env, sizeof(jmp_buf));
}

  inline static 
void freeze_worker_state(__cilkrts_worker *w, __cilkrts_paused_stack *pstk)
{
  pstk->w                       = w;
  pstk->ff                      = w->l->frame_ff;
  pstk->team                    = w->l->team;
  pstk->flags                   = w->current_stack_frame->flags;
  pstk->current_stack_frame     = w->current_stack_frame;
  memcpy(&pstk->env, &w->l->env, sizeof(jmp_buf));
}

/**
 * Restore the context of a paused worker. 
 */
inline static void thaw_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk) 
{
  jmp_buf *ctx = &(pstk->ctx);
  cilk_dbg(IVAR,"[thaw_frame] thawing paused stack: %p\n", pstk);

  thaw_worker_state(w, pstk);

  longjmp(*ctx, 1);
  CILK_ASSERT(0); //does not return
}

//ASSERT: frame lock owned
inline static void freeze_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk)
{
  full_frame *ff            = w->l->frame_ff;
  __cilkrts_stack_frame *sf = w->current_stack_frame;

  cilk_dbg(IVAR,"[freeze_frame] freezing paused stack: %p\n", pstk);

  CILK_ASSERT(ff);
  CILK_ASSERT(sf);

  freeze_worker_state(w,pstk);

  //the suspended flag is set to maintain the contract with cilk frame state. 
  sf->flags |= CILK_FRAME_SUSPENDED;

  /* CALL_STACK becomes valid again */
  ff->call_stack = sf;
  __cilkrts_put_stack(ff, sf);
}

void restore_ready_computation(__cilkrts_worker *w)
{
  __cilkrts_paused_stack *pstk = NULL;
  dequeue(w->ready_queue, (ELEMENT_TYPE *) pstk);
  if (pstk) {
    thaw_frame(w, pstk);
    CILK_ASSERT(0); //no return
  }
}


/**
 * Commits a pause of an IVar. 
 * The steps involved in committing a pause are as follows:
 * 1. Copy the relevent state of the worker into a paused stack structure.
 *    This structure should contain all the state needed for another worker
 *    to pick up the current execution context once the IVar is full.
 * 2. It is the responsibility of the caller to call the appropriate scheduler.
 *    e.g.  __cilkrts_run_scheduler_with_exceptions(&concurrent_sched, w, NULL);
 */
CILK_API(void) __cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack *pstk) 
{
  full_frame *ff = w->l->frame_ff;

  BEGIN_WITH_WORKER_AND_FRAME_LOCK(w, ff) {
    freeze_frame(w, pstk);
  } END_WITH_WORKER_AND_FRAME_LOCK(w, ff);

  //don't forget to call the scheduler!
}



//-------------------------------

void concurrent_sync(__cilkrts_worker *w) 
{
  return;
}

//-------------------------------

int paused_stack_trylock(__cilkrts_paused_stack *pstk) { return cas(&(pstk->lock), 0, 1); }
void paused_stack_unlock(__cilkrts_paused_stack *pstk) { atomic_release(&(pstk->lock), 0); }

//------------------------------------------------------------------------
//hrm...I don't like these being here...todo: find a home for these sleep functions.
CILK_API(void) __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wimplicit-function-declaration"
#endif
  nanosleep(&time,NULL);
#ifdef __clang__
#pragma clang diagnostic pop
#endif
}

CILK_API(void) __cilkrts_usleep(unsigned long micros)
{
  struct timespec time;
  time.tv_sec = micros/1000000ul;
  time.tv_nsec = (micros % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

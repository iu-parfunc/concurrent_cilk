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
#include <bug/bug.h>
#include "os.h"
#include "os_mutex.h"
#include "local_state.h"
#include "signal_node.h"
#include "full_frame.h"
#include "stacks.h"
#include "sysdep.h"
#include "except.h"
#include <malloc/cilk_malloc.h>
#include "pedigrees.h"
#include <util/cilk_util.h>

#include <cilk/concurrent_queue.h>
#include <setjmp.h>
#include <cilk/concurrent_cilk.h>
#include "concurrent_cilk_internal.h"

//for nanosleep "declared implicity"...it should be in time.h but isn't?
#pragma warning(disable: 266)

//for atomic_release
#pragma warning(disable: 2206)

coldspot
CILK_API(void) __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

coldspot
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
  CILK_ASSERT(ivar);
  *ivar = 0;
}

  static void 
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

  static
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
void thaw_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk) 
{
  jmp_buf *ctx = &(pstk->ctx);
  IVAR_DBG_PRINT(1,"thawing pstk: %p\n", pstk);

  thaw_worker_state(w, pstk);

  longjmp(*ctx, 1);
  CILK_ASSERT(0); //does not return
}

//ASSERT: frame lock owned
void freeze_frame(__cilkrts_worker *w, __cilkrts_paused_stack *pstk)
{
  full_frame *ff            = w->l->frame_ff;
  __cilkrts_stack_frame *sf = w->current_stack_frame;

  CILK_ASSERT(ff);
  CILK_ASSERT(sf);

  freeze_worker_state(w,pstk);

  //the suspended flag is set to maintain the contract with cilk frame state. 
  sf->flags |= CILK_FRAME_SUSPENDED;

  /* CALL_STACK becomes valid again */
  ff->call_stack = sf;
  __cilkrts_put_stack(ff, sf);
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

int paused_stack_trylock(__cilkrts_paused_stack *pstk) {
  return cas(&(pstk->lock), 0, 1);
}

void paused_stack_unlock(__cilkrts_paused_stack *pstk) {
  atomic_release(&(pstk->lock), 0);
}

int setup_restore_queue(__cilkrts_worker *w, __cilkrts_worker *victim)
{
   uintptr_t ptr;

  //verify that w is in a good state to receive a new restoration queue
  //NOTE; The order here cannot be switched. a 0 return says "try to do_restoration"
  //and anything else says "return immediately to calling context"
  if (w->restore_queue) {
    if (q_is_empty(w->restore_queue)) {
      CILK_ASSERT(w->escape);
      CILK_ASSERT(w->escape->w);
      delete_stack_queue(w->restore_queue); //TODO: cache?
      w->restore_queue = NULL;              //The restore queue is now invalid
      thaw_frame(w, w->escape);             //Escape context restored
      CILK_ASSERT(0);
    } else {
      return 0;
    }
  }

  if (q_is_empty(victim->ready_queue)) return -1;

  steal_queue(w, victim); //steal the ready_queue off the victim

  //the pause must happen here because this is the last pointer the worker
  //can view its original context before its restoration cycle starts.
  ptr = __cilkrts_pause(w->escape->ctx);

  //executes twice: ptr == 0 on pause, ptr == 1 on longjmp
  if (! ptr) __cilkrts_finalize_pause(w, w->escape); 

  return ptr;
}

void do_restoration(__cilkrts_worker *w)
{
  __cilkrts_paused_stack *tmp = NULL;
  CILK_ASSERT(w->restore_queue);

  dequeue(w->restore_queue, (ELEMENT_TYPE *) &tmp); 
  CILK_ASSERT(tmp); 
  thaw_frame(w, tmp);                     //resumes tmp's context
  CILK_ASSERT(0);                          
}

void restore_ready_computation(__cilkrts_worker *w) 
{
  if (setup_restore_queue(w, w)) return;
  do_restoration(w);
}

queue_t *replace_queue(__cilkrts_worker *w)
{
  queue_t *fresh_queue = (queue_t *) make_stack_queue();
  queue_t *ready_queue = w->ready_queue;
  if (cas(&(w->ready_queue), ready_queue, fresh_queue)) {
    return ready_queue;
  }
  return NULL;
}

/**
 * Try to steal the ready queue for restoration from a worker. 
 * This will populate w->restore_queue and provide a new queue for 
 * the victim. The escape context has its pointer set here, but the
 * user can change the escape pointer whenever they wish. 
 */
int steal_queue(__cilkrts_worker *thief, __cilkrts_worker *victim)
{
  __cilkrts_paused_stack *escape = thief->escape;
  queue_t *q;
  full_frame *ff = thief->l->frame_ff;

  //short circuit if there is already a queue waiting to be restored.
  if (thief->restore_queue) return 0;

  //swap the ready_queue out for a shiny new queue
  q = replace_queue(victim);

  //someone else got the queue...don't continue.
  if (! q) return 0;

  CILK_ASSERT(thief->restore_queue == NULL);

  //install the old ready_queue as the restor_queue
  thief->restore_queue = q;

  return 1;
}


// Used in cilk-abi.c in return_frame
//-------------------------------------
//whenever a non blocked frame returns, we check to see if it can perform a
//pop on some blocked work. The preference is to clear the worker's blocked
//queue.
void __concurrent_cilk_leave_frame_hook(__cilkrts_worker *w, __cilkrts_stack_frame *sf)
{
  //turn on restoration of full ivars on leave frame
#define CILK_RESTORATION_POINT_LEAVE_FRAME
#ifdef CILK_RESTORATION_POINT_LEAVE_FRAME
  //restore everything from the restore queue
  restore_ready_computation(w);
#endif

  return;
}
//------------------------------------------


#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include <cilk/concurrent_queue.h>
#include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>
#include "scheduler.h"
#include <bug/bug.h>

inline
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar *ivar)
{
  //fast path -- already got a value
  if(IVAR_READY(*ivar)) return UNTAG(*ivar);

  //slow path -- ivar must wait for a value
  return slow_path(ivar);
}

ivar_payload_t slow_path(__cilkrts_ivar *ivar)
{
  __cilkrts_worker *w;
  __cilkrts_paused_stack pstk = {0};
  __cilkrts_paused_stack *peek;
  __cilkrts_paused_stack *pstk_head;
  uintptr_t ptr;

  //slow path -- operation must block until a value is available
  w = __cilkrts_get_tls_worker_fast();

  //before trying to pause a computation, clear the dequeue
  //of the worker to see if continuing blocked work will fill this ivar.
#define CILK_RESTORATION_POINT_IVAR_BLOCK
#ifdef CILK_RESTORATION_POINT_IVAR_BLOCK
  restore_ready_computation(w);
#endif

  if(IVAR_READY(*ivar)) { return UNTAG(*ivar); }

  ptr = (uintptr_t) __cilkrts_pause(pstk.ctx);

  if(! ptr) {

    // Register the continuation in the ivar's header.
    do {

      if(IVAR_PAUSED(*ivar)) {
       IVAR_DBG_PRINT(1,"ivar read in paused state\n");

        //pull out the reference to the root paused stack
        //all reads on a paused ivar compete for the lock on this
        //paused stack
        pstk_head = (__cilkrts_paused_stack  *) (*ivar >> IVAR_SHIFT);

        //set the tail element to be the new paused stack
        if(paused_stack_trylock(pstk_head)) {

          //append the new paused stack to the waitlist 
          pstk_head->tail->next = &pstk; 

          //the new stack is the new tail
          pstk_head->tail = &pstk;

          paused_stack_unlock(pstk_head);

          //go directly to finalize pause
          break; //<<< at this point we commit to finalizing the pause.
        }

      }

      // Well nevermind then... now it is full.
      if(IVAR_READY(*ivar)) return UNTAG(*ivar);

      //do we need this lock?
      //maybe it can be refactored into 1 lock with the above
      if(paused_stack_trylock(pstk_head)) {
        //queue must be empty, so our paused stack becomes the head and tail.
        pstk.head = &pstk;
        pstk.tail = &pstk;
        paused_stack_unlock(pstk_head);
      }

    } while(! cas(ivar, 0, (((ivar_payload_t) &pstk) << IVAR_SHIFT) | CILK_IVAR_PAUSED));

    __cilkrts_finalize_pause(w, &pstk); 
    __cilkrts_run_scheduler_with_exceptions(&concurrent_sched, w, NULL);
    CILK_ASSERT(0); //no return. heads to scheduler.
  }

  // <-- We only jump back to here when the value is ready.

  IVAR_DBG_PRINT(1,"ivar returning\n");
  return UNTAG(*ivar);
}

  inline CILK_API(void)
__cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  ivar_payload_t new_val, old_val;
  __cilkrts_paused_stack  *pstk;
  
  //the new value is the actual value with the full ivar tag added to it
  new_val = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
  old_val = (ivar_payload_t) atomic_set(ivar, new_val);

  if (IVAR_PAUSED(old_val)) {
    pstk = (__cilkrts_paused_stack *) (old_val >> IVAR_SHIFT);
    IVAR_DBG_PRINT(1,"enqueueing ctx %p\n", pstk->w->ready_queue);

    //this is thread safe because any other reads of the ivar take the fast path.
    //Therefore the waitlist of paused stacks can only be referenced here.
    do {
      enqueue(pstk->w->ready_queue, (ELEMENT_TYPE) pstk);
      pstk = pstk->next;
    } while(pstk);

  }

  if_f(IVAR_READY(old_val)) 
    __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
}

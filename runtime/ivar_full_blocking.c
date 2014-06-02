#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include "concurrent_queue.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>
#include "scheduler.h"
#include "bug.h"

/** 
 * Clear an IVar.
 *
 * Sets the IVar's value to 0. 
 */
inline CILK_API(void)
__cilkrts_ivar_clear(__cilkrts_ivar* ivar) { *ivar = 0; }

/**
 * Read an IVar and obtain its value. An IVar can be in any one of three states:
 *  CASE 1:IVar is full:
 *          -- return the value.
 *  CASE 2: IVar is already in a paused state:
 *          -- register the continuation as waiting on the IVar's value.
 *          -- pause the continuation and steal work.
 *          -- once the IVar is full, the function will return the value.
 *  CASE 3: IVar is empty:
 *         -- atomically install this continuation's paused stack and set the tag to CILK_IVAR_PAUSED.
 *         -- pause the continuation and steal work.
 *         -- once the IVar is full, the function will return the value.
 */
inline CILK_API(ivar_payload_t)
__cilkrts_ivar_read(__cilkrts_ivar *ivar)
{
  __cilkrts_worker *w;
  __cilkrts_paused_fiber *volatile pfiber_head;
  __cilkrts_paused_fiber pfiber;
  unsigned short exit = 0;
  uintptr_t old_w;

  //fast path -- already got a value.
  if(IVAR_READY(*ivar)) { return UNTAG(*ivar); }

  //slow path -- operation must block until a value is available.
  memset(&pfiber, 0, sizeof(__cilkrts_paused_fiber));
  w     = __cilkrts_get_tls_worker_fast();
  old_w = (uintptr_t) __cilkrts_pause_fiber(pfiber->ctx);

  if(! old_w) {

    do {
      switch (*ivar & IVAR_MASK) {
        case CILK_IVAR_EMPTY: 
          pfiber.head = pfiber.tail = &pfiber;
          //if the CAS operation succeeds, the ivar is now in the paused state,
          //and this continuation is waiting on it. 
          exit = cas(ivar, 0, (((ivar_payload_t) &pfiber) << IVAR_SHIFT) | CILK_IVAR_PAUSED); 
          break;
        case CILK_IVAR_PAUSED: 
          //pull out the reference to the root paused stack
          //all reads on a paused ivar compete for the lock on this
          //paused stack
          pfiber_head = (__cilkrts_paused_fiber  *) (*ivar >> IVAR_SHIFT);

          //set the tail element to be the new paused stack
          while (! paused_fiber_trylock(pfiber_head)) spin_pause();
          //append the new paused stack to the waitlist 
          pfiber_head->tail->next = pfiber_head->tail = &pfiber;
          paused_fiber_unlock(pfiber_head);
          exit = 1;
          break;
        default: 
          __cilkrts_bug("Cilk IVar %p in corrupted state. Aborting program.\n", ivar);
      }
    } while(!exit);

    __cilkrts_commit_pause(&pfiber); 
    CILK_ASSERT(0); //no return. heads to scheduler.
  }

  return UNTAG(*ivar);
}


/**
 * Write an Ivar with an opaque value. 
 * Multiple writes are treated as a bug. 
 */
inline CILK_API(void)
__cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  __cilkrts_paused_fiber *pfiber;
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t new_val  = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
  ivar_payload_t old_val  = (ivar_payload_t) atomic_set(ivar, new_val);

  switch (old_val & IVAR_MASK) {
    case CILK_IVAR_PAUSED:
      pfiber = (__cilkrts_paused_fiber *) (old_val >> IVAR_SHIFT);
      //this is thread safe because any other reads of the ivar take the fast path.
      //Therefore the waitlist of paused stacks can only be referenced here.
      do {
        //TODO: no need to maintain one queue...probably more efficiecient to keep a ** array in the worker 
        enqueue(pfiber->w->ready_queue, (ELEMENT_TYPE) pfiber);
        pfiber = pfiber->next;
      } while(pfiber);
    case CILK_IVAR_EMPTY:
      break;  //do nothing -- write already done.
    case CILK_IVAR_FULL:
      __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
    default:
      __cilkrts_bug("Cilk IVar %p in corrupted state. Aborting program.\n", ivar);

  }
}

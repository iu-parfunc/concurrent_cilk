#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include "concurrent_queue.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include "scheduler.h"
#include "bug.h"

inline
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar *ivar)
{
  __cilkrts_worker *w;
  __cilkrts_paused_stack *pstk;
  uintptr_t ptr;
  //fast path -- already got a value
  if(IVAR_READY(*ivar)) {
    return UNTAG(*ivar);
  }

  pstk = __cilkrts_malloc(sizeof(__cilkrts_paused_stack));

  //slow path -- operation must block until a value is available
  w = __cilkrts_get_tls_worker_fast();
  ptr = (uintptr_t) __cilkrts_pause((pstk->ctx));

  if(! ptr) {

    // Register the continuation in the ivar's header.
    do {

      if(IVAR_PAUSED(*ivar)) break;

      if(IVAR_READY(*ivar)) {
        // Well nevermind then... now it is full.
        return UNTAG(*ivar);
      }

    } while(! cas(ivar, 0, (((ivar_payload_t) pstk) << IVAR_SHIFT) | CILK_IVAR_PAUSED));

    __cilkrts_finalize_pause(w, pstk); 
     CILK_ASSERT(0); //no return. heads to scheduler.
  }

  // <-- We only jump back to here when the value is ready.

  printf("ivar returning\n");
  return UNTAG(*ivar);
}

  inline CILK_API(void)
__cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t newval = (val << IVAR_SHIFT) | CILK_IVAR_FULL;

  //write without checking for any checking. Either this succeeds or there is a bug
  ivar_payload_t old_val = (ivar_payload_t) atomic_set(ivar, newval);

  if(IVAR_PAUSED(old_val)) {
    __cilkrts_paused_stack *pstk = (__cilkrts_paused_stack *) (old_val >> IVAR_SHIFT);
    printf("enqueueing ctx %p\n", pstk->w->ready_queue);
    enqueue(pstk->w->ready_queue, (ELEMENT_TYPE) pstk);
  }

  if(IVAR_READY(old_val)) 
    __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
}

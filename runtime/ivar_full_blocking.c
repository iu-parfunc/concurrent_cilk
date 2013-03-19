#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include "scheduler.h"
#include "bug.h"
#include "jmpbuf.h"

#define IVAR_SHIFT 0x4
#define IVAR_READY(iv) (iv & 0xf) == 1
#define TAG(iv)   (iv << IVAR_SHIFT)
#define UNTAG(iv) (iv >> IVAR_SHIFT)

inline
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar *iv)
{
  //multiple threads (reader(s) + writer modify iv). make a volatile copy
  volatile __cilkrts_ivar *ivar = iv;
  volatile __cilkrts_paused_stack *ptr;
  __cilkrts_worker *wkr, *new_w;

  //fast path -- already got a value
  if(IVAR_READY(*ivar)) {
    return UNTAG(*ivar);
  }

  CILK_ASSERT(*ivar == 0);
  //slow path -- operation must block until a value is available
  wkr = __cilkrts_get_tls_worker_fast();
  ptr = __cilkrts_pause(wkr);

  //SETUP CONTINUATION
  //---------------------------------------------
  if((unsigned long) ptr) {

    // Register the continuation in the ivar's header.
    printf("worker %d/%p pausing stack %p\n", wkr->self, wkr, ptr);
    do {

      if(IVAR_READY(*ivar)) {

     //    printf("DING! ivar got written to while we were pausing abort abort! %p\n", ptr);
        // Well nevermind then... now it is full.
        __cilkrts_undo_pause(wkr,ptr);
        return UNTAG(*ivar);
      }
    } while(! cas(ivar, 0, ((ivar_payload_t) ptr) << IVAR_SHIFT));

    //FINALIZE THE PAUSE
    //---------------------------------------------

    // create a new replacement worker:
    new_w = get_replacement_worker(wkr);
    __cilkrts_set_tls_worker(new_w);
    __cilkrts_fence();

    //register the replacement worker for stealing and
    //additionally have the replacement inherit the parent's
    //forwarding array.
    add_replacement_worker(wkr, new_w, ptr);


    printf("old_w %d/%p going to sched with worker: %d/%p\n", wkr->self, wkr, new_w->self, new_w);
    __cilkrts_run_scheduler_with_exceptions(new_w); // calls __cilkrts_scheduler
    CILK_ASSERT(0); //should never get here
  }

  // <-- We only jump back to here when the value is ready.
  return UNTAG(*ivar);
}

  inline
CILK_API(void) __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();
  ivar_payload_t peek = *ivar;

  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t newval = (val << IVAR_SHIFT) | CILK_IVAR_FULL;

   while (! cas(ivar, peek, newval)) {
     __cilkrts_short_pause(); //wait 16 cycles
     peek = *ivar; //try again
   }
   //printf("worker %d/%p write ivar %p with val %lu\n", w->self, w, ivar, UNTAG(newval));

  //DESIGN DECISION: One could allow multiple puts of the same value.  Not doing so for now.
  if(! peek) return;
  if_f(IVAR_READY(peek)) 
    __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);

  __cilkrts_fence();
  __cilkrts_wake_stack((volatile __cilkrts_paused_stack *) UNTAG(peek));
}

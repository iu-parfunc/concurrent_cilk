#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include "concurrent_queue.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>
#include <cilk/concurrent_queue.h>
#include "scheduler.h"
#include "sysdep.h"
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
  __cilkrts_worker *w, *replacement;
  unsigned short exit = 0;
  uintptr_t val;
  jmp_buf ctx; 
  uintptr_t volatile peek;
  ivar_payload_t payload;
  queue_t *waitlist = NULL;

  CILK_ASSERT(ivar);

  //fast path -- already got a value.
  //----------------------------------
  if (IVAR_READY(*ivar)) {
    dbgprint(IVAR, "ivar %p FULL -fast path\n", ivar);
    return UNTAG(*ivar);
  }

  //slow path -- operation must block until a value is available.
  //----------------------------------
  val = (uintptr_t) __cilkrts_pause_fiber((struct __jmp_buf_tag *)&ctx);

  if (! val) {
    w = __cilkrts_get_tls_worker_fast();
    w->paused_ctx  = &ctx;
    CILK_ASSERT(w->current_stack_frame);
    replacement = __cilkrts_commit_pause(w); 

    do {
      peek = *ivar;
      switch (peek & IVAR_MASK) {
        case CILK_IVAR_EMPTY: 
          waitlist = make_stack_queue();
          payload  = (((ivar_payload_t) waitlist) << IVAR_SHIFT) | CILK_IVAR_PAUSED;
          enqueue(waitlist, (ELEMENT_TYPE) w);
          exit = cas(ivar, 0, payload); 
          if (! exit) { 
            delete_stack_queue(waitlist);
            waitlist = NULL; 
            dbgprint(IVAR, "ivar %p failed cas on EMPTY ivar - going around again\n", ivar);
          } else {
            dbgprint(IVAR, "ivar %p EMPTY. Filled with replacement %p\n", ivar, replacement);
          }
          break;
        case CILK_IVAR_PAUSED:
          waitlist = (queue_t *)(*ivar >> IVAR_SHIFT);
          CILK_ASSERT(waitlist);
          exit = cas(ivar, peek, (((ivar_payload_t) waitlist) << IVAR_SHIFT) | CILK_IVAR_LOCKED);
          if (exit) {
            enqueue(waitlist, (ELEMENT_TYPE) w);
            //no cas needed, we hold the lock, which is now released. 
            *ivar = (((ivar_payload_t) waitlist) << IVAR_SHIFT) | CILK_IVAR_PAUSED;
            dbgprint(IVAR,"ivar %p PAUSED. adding worker %p to waitlist %p\n",ivar,w,waitlist);
            break;
          } 
        case CILK_IVAR_FULL:
          dbgprint(IVAR, "ivar %p FILLED while reading\n", ivar);
          //nevermind...someone filled it. 
          //__cilkrts_free(replacement); //TODO: cache
          return UNTAG(*ivar);
          break; //go around again
        case CILK_IVAR_LOCKED:
          break; //go around again
        default: 
          __cilkrts_bug("[read] Cilk IVar %p in corrupted state 0x%x. Aborting program.\n", ivar, *ivar&IVAR_MASK);
      }
    } while (!exit);

    //thread local array operation, no lock needed
    register_worker_for_stealing(w);

    //sets pthread TLS to replacement worker and invokes the scheduler.
    __cilkrts_worker_stub((void *) replacement);
    CILK_ASSERT(0); //no return. heads to scheduler.
  }

  return UNTAG(*ivar);
}


/**
 * Write an Ivar with an opaque value. 
 * Multiple writes are treated as a bug. 
 */
  inline CILK_API(void)
__cilkrts_ivar_write(__cilkrts_ivar *ivar, ivar_payload_t val) 
{
  __cilkrts_worker *w;
  unsigned short exit = 0;
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t new_val  = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
  ivar_payload_t old_val; 
  ivar_payload_t volatile peek; 
  queue_t *waitlist = NULL; 

  CILK_ASSERT(ivar);
  do {
    peek = *ivar;
    switch (peek & IVAR_MASK) {
      case CILK_IVAR_PAUSED:
        dbgprint(IVAR, "filling paused ivar %p\n", ivar);
        old_val = casv(ivar, peek, new_val);
        if (old_val != peek) { break; }


        CILK_ASSERT(old_val == peek);
        waitlist = (queue_t *) (old_val >> IVAR_SHIFT);
        CILK_ASSERT(waitlist);
        while (! dequeue(waitlist, (ELEMENT_TYPE *) &w)) {
          dbgprint(IVAR, "enqueuing %p on readylist %p sf %p\n",
              w, w->readylist, w->current_stack_frame);
          enqueue(w->readylist, (ELEMENT_TYPE) w);
        }
        exit = 1;
        break;
      case CILK_IVAR_EMPTY:
        dbgprint(IVAR, "filling empty ivar %p\n", ivar);
        old_val = casv(ivar, peek, new_val);
        printf("old_val 0x%lx flags 0x%lx\n", ((uintptr_t) old_val >> IVAR_SHIFT), old_val & IVAR_MASK);
        if ((*ivar & IVAR_MASK) == CILK_IVAR_FULL)  { exit = 1; }
        break;  
      case CILK_IVAR_LOCKED:
        break; //go around again
      case CILK_IVAR_FULL:
        __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
      default:
        __cilkrts_bug("[write] Cilk IVar %p in corrupted state 0x%x. Aborting program.\n",
            ivar, *ivar&IVAR_MASK);
    }
  } while(!exit);
}

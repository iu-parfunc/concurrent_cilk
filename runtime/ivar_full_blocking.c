#include <stdlib.h>
#include <string.h>
#include "concurrent_cilk_internal.h"
#include "concurrent_queue.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>
#include <cilk/concurrent_queue.h>
#include "scheduler.h"
#include "sysdep.h"
#include "bug.h"

void initialize_stack_queue(queue_t *q);
void clear_stack_queue(queue_t* q);

inline CILK_API(void)
__cilkrts_ivar_clear(__cilkrts_ivar* ivar) { *ivar = 0; __sync_synchronize(); }

inline CILK_API(void)
__cilkrts_ivar_array_clear(__cilkrts_ivar *ptr, int size)
{ 
  memset(ptr, 0, sizeof(__cilkrts_ivar) * (size_t) size);
  __sync_synchronize();
}

inline CILK_API(__cilkrts_ivar*)
__cilkrts_new_ivar_array(int size)
{
  __cilkrts_ivar* ptr = calloc(sizeof(__cilkrts_ivar), size);
  __sync_synchronize();
  return ptr;
}

#ifdef IVAR_BUSYWAIT_VARIANT
#include "ivar_busywait.c"
#else 

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
  uintptr_t val;
  jmp_buf ctx; 
  uintptr_t volatile peek;

  CILK_ASSERT(ivar);

  //fast path -- already got a value.
  //----------------------------------
  if (IVAR_READY(*ivar)) {
    dbgprint(IVAR, "ivar %p FULL -fast path\n", ivar);
    return UNTAG(*ivar);
  }

  //slow path -- operation must block until a value is available.
  //----------------------------------
  val = (uintptr_t) __cilkrts_pause_fiber(&ctx);

  if (! val) {
    w = __cilkrts_get_tls_worker_fast();
    replacement = __cilkrts_commit_pause(w, &ctx); 

    peek = *ivar;
    switch (peek & IVAR_MASK) {
      case CILK_IVAR_EMPTY: 
        dbgprint(IVAR, "ivar %p read in empty state by worker %d/%p\n", ivar, w->self, w);
        w->ready_flag = (uintptr_t *) ivar;
        break;
      case CILK_IVAR_FULL:
        dbgprint(IVAR, "ivar %p FILLED while reading\n", ivar);
        //nevermind...someone filled it. 
        __cilkrts_roll_back_pause(w, replacement);
        return UNTAG(*ivar);
      default: 
        __cilkrts_bug("[read] Cilk IVar %p in corrupted state 0x%x. Aborting.\n", ivar, *ivar&IVAR_MASK);
    }

    //thread local array operation, no lock needed
    __cilkrts_register_paused_worker_for_stealing(w);
    __cilkrts_run_replacement_fiber(replacement);
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
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t new_val  = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
  ivar_payload_t volatile peek; 

  CILK_ASSERT(ivar);
  peek = *ivar;
  switch (peek & IVAR_MASK) {
    case CILK_IVAR_EMPTY:
      dbgprint(IVAR, "filling empty ivar %p\n", ivar);
      if_f(!cas(ivar, peek, new_val)) {
        __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
      }
      break;  
    case CILK_IVAR_FULL:
      __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
    default:
      __cilkrts_bug("[write] Cilk IVar %p in corrupted state 0x%x. Aborting.\n", ivar, *ivar&IVAR_MASK);
  }
}

#endif // if IVAR_BUSYWAIT_VARIANT


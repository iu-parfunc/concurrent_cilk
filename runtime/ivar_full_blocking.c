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


struct cons {
  // ELEMENT_TYPE car; 
  __cilkrts_worker * car;
  struct cons* cdr;
};
typedef struct cons cons_t;

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
  ivar_payload_t my_payload;

  cons_t my_waitlist_cell, *cur_cell;

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
    my_waitlist_cell.car = w;
    my_payload = (((ivar_payload_t) &my_waitlist_cell) << IVAR_SHIFT) | CILK_IVAR_PAUSED;
    replacement = __cilkrts_commit_pause(w, &ctx); 

    do {
      peek = *ivar;
      switch (peek & IVAR_MASK) {
        case CILK_IVAR_EMPTY: 
          my_waitlist_cell.cdr = NULL;          
          exit = cas(ivar, 0, my_payload);
          if (! exit) { 
            dbgprint(IVAR, "ivar %p failed cas on EMPTY ivar - going around again\n", ivar);
          } else {
            dbgprint(IVAR, "ivar %p EMPTY. Filled with replacement %p\n", ivar, replacement);
          }
          break;
        case CILK_IVAR_PAUSED:
          cur_cell = (cons_t *)(peek >> IVAR_SHIFT);
          CILK_ASSERT(cur_cell); // Never empty because it starts as a singleton
	  my_waitlist_cell.cdr = cur_cell; // If we bump them with CAS, then they are our neighor.
          exit = cas(ivar, peek, my_payload);
          if (exit) dbgprint(IVAR,"ivar %p PAUSED. added worker %p to waitlist %p\n",ivar,w,waitlist);
          break;
        case CILK_IVAR_FULL:
          dbgprint(IVAR, "ivar %p FILLED while reading\n", ivar);
          //nevermind...someone filled it. 
          __cilkrts_roll_back_pause(w, replacement);
          return UNTAG(*ivar);
          break; //go around again
        default: 
          __cilkrts_bug("[read] Cilk IVar %p in corrupted state 0x%x. Aborting program.\n", ivar, *ivar&IVAR_MASK);
      }
    } while (!exit);

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
  __cilkrts_worker *w;
  unsigned short exit = 0;
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t new_val  = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
  ivar_payload_t old_val; 
  ivar_payload_t volatile peek; 
  cons_t *waitlist = NULL; 

  CILK_ASSERT(ivar);
  do {
    peek = *ivar;
    switch (peek & IVAR_MASK) {
      case CILK_IVAR_PAUSED:
        dbgprint(IVAR, "filling paused ivar %p\n", ivar);
        old_val = casv(ivar, peek, new_val);
        if (old_val != peek) { break; }


        CILK_ASSERT(old_val == peek);
        // We own the whole waitlist here:
        waitlist = (cons_t *) (old_val >> IVAR_SHIFT);
        CILK_ASSERT(waitlist);
        while ( waitlist != NULL ) {
          w = waitlist->car;
          dbgprint(IVAR, "enqueuing %p on readylist %p sf %p\n",
              w, w->readylist, w->current_stack_frame);
          enqueue(w->readylist, (ELEMENT_TYPE) w);
          waitlist = waitlist->cdr;
        }
        exit = 1;
        break;
      case CILK_IVAR_EMPTY:
        dbgprint(IVAR, "filling empty ivar %p\n", ivar);
        old_val = casv(ivar, peek, new_val);
        dbgprint(IVAR, "old_val 0x%lx flags 0x%lx\n", ((uintptr_t) old_val >> IVAR_SHIFT), old_val & IVAR_MASK);
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

#endif // if IVAR_BUSYWAIT_VARIANT


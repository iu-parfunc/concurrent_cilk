// NOTE: Presently this code is CONDITIONALLY included/compiled into another file.

// Thus this whole file is implicitly IFDEF CILK_IVARS

// ====================================================================================================

// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.

// API & Usage notes:
// 
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.
//
//  Paused stacks are represented by pointers.  Presently they may only be woken once.
//  (Waking frees the heap object containing the paused stack.)


// For my_resume only (SP): 
#include "jmpbuf.h"
#include <pthread.h>
#include <stdio.h>
#include "local_state.h"
#include "scheduler.h"
#include <time.h>

#include "concurrent_cilk_internal.h"
#include "concurrent_cilk_forwarding_array.h"
#include <concurrent_queue.h>

#   define max(a, b) ((a) < (b) ? (b) : (a))

extern unsigned myrand(__cilkrts_worker *w);


int can_steal_from(__cilkrts_worker *victim);

#   pragma warning(disable:266)   // disable warning that nanosleep is implicitely defined. it SHOULD be in time.h...
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

inline
CILK_API(void) __cilkrts_ivar_clear(__cilkrts_ivar* ivar)
{
  *ivar = 0;
}


//restore the context of a paused worker
void restore_paused_worker(__cilkrts_worker *ready_w) 
{
  //if the worker came into the scheduler
  //there should be no more work left to de
  //since we don't preempt our threads
  CILK_ASSERT(! can_steal_from(ready_w));
  //we better be here because the ivar was written to
  CILK_ASSERT(ready_w->ivar);
  CILK_ASSERT(IVAR_READY(*ready_w->ivar));

    //if(! cas(&ready_w->ivar, ready_w->ivar, NULL)) {
    //  __cilkrts_bug("wtf\n");
    //}
    ready_w->ivar = NULL;

  //restore the original blocked worker
  //at this point. the scheduler forgets about tlsw
  __cilkrts_set_tls_worker(ready_w);

  //--------------------------- restore the context -------------------
  CILK_ASSERT(ready_w->ctx);
  CILK_LONGJMP(ready_w->ctx);

  //does not return
  CILK_ASSERT(0);
}

// This initiializes the data structure that represents a paused stack, but save_worker
// must be called subsequently to fully populate it.
int make_paused_stack(__cilkrts_worker* w,  __cilkrts_ivar *ivar) 
{
  CILK_ASSERT(w->ivar == NULL);
  //cas(&w->ivar, NULL, ivar);
  w->ivar = ivar;
  CILK_ASSERT(w->ivar);
  return 1;
}

// Commit the pause.
  inline
CILK_API(__cilkrts_worker *) __cilkrts_finalize_pause(__cilkrts_worker* w) 
{
  //printf("<<after>> w %p fibre %p\n",w, stk);
  // create a new replacement worker:
  __cilkrts_worker* new_w = get_replacement_worker(w);
  __cilkrts_fence();

  //register the replacement worker for stealing and
  //additionally have the replacement inherit the parent's
  //forwarding array.
  inherit_forwarding_array(w, new_w);

  //printf("pause finalized: w %d/%p paused and replacement %d/%p setup\n",w->self, w, new_w->self, new_w);
  // head to the scheduler with the replacement worker
  return new_w;
//  __cilkrts_run_scheduler_with_exceptions(new_w); // calls __cilkrts_scheduler
//  CILK_ASSERT(0); //no return
}

// Back out of the pause before making any externally visible changes.
// The client better not have stored the __cilkrts_paused_stack anywhere!
inline
//CILK_API(void) __cilkrts_undo_pause(__cilkrts_worker *w, __cilkrts_paused_stack* stk) 
CILK_API(void) __cilkrts_undo_pause(__cilkrts_worker *w) 
{
  CILK_ASSERT(w->ivar);
  w->ivar = 0;
  __cilkrts_fence();
  //cache the paused stack for reuse
  //enqueue(w->paused_stack_cache, (ELEMENT_TYPE) stk);
}

// Mark a paused stack as ready by populating the workers pstk pointer.
// multiple writes are idempotent
  inline
CILK_API(void) __cilkrts_wake_stack(PAUSED_FIBER stk)
{
  //stk->ready = 1;
  __cilkrts_fence();
}

__cilkrts_worker *pick_random_victim(__cilkrts_worker *w)
{
  int m, n;
  volatile __cilkrts_forwarding_array *arr = w->forwarding_array;

  __cilkrts_worker *res;
  __cilkrts_forwarding_array *selection;

    selection = NULL;
    //select a new victim by randomly selecting a forwarding array
    //and then randomly selecting an array slot within that

    for(m=0; m < *arr->capacity; m++) {
      if(arr->links[m]->elems > 1) {
        selection = arr->links[m];
        break;
      }
    }

    if(!selection) {
      return arr->ptrs[ARRAY_SIZE-1];
    }

    n = (myrand(w) % ((ARRAY_SIZE) - arr->leftmost_idx));
    n = arr->leftmost_idx+n;

    //m maps to the array of forwarding arrays. 
    //n maps to a victim candidate location in the chosen forwarding array
    //together, these gain us a new victim (possibly null)

    res = (arr->links[m])->ptrs[n];

    
    if(res) {

      //instead, perform maintenence if there is no way this worker
      //has any chance of restorationntenence routine to cache workers who have no hope of ever
      //getting restored because they were just extra workers to take 
      //over for paused ones that are no longer needed
      if(res->ivar == NULL && !res->ctx && res->self == -1) {

        //remove the worker <SAFETY> using cas here...probably don't need it
        //if(! cas(&arr->links[m]->ptrs[n],res,NULL)) abort();
        arr->links[m]->ptrs[n] = NULL;
        arr->links[m]->elems--;
        enqueue(w->worker_cache, (ELEMENT_TYPE) res); 
        return NULL;
      }
    }

  return res;
}


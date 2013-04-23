#include "scheduler.h"
#include "concurrent_cilk_forwarding_array.h"
#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#include "cilk_malloc.h"
#include "worker_mutex.h"
#include "concurrent_queue.h"


 /**
  * Create a new worker.
  *
  */  
void setup_new_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker) 
{   
  CILK_ASSERT(old_w);
  CILK_ASSERT(fresh_worker);

  // WARNING: FUTURE CILKSCREEN SUPPORT WOULD REQUIRE MORE WORK HERE:
  // __cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker + sizeof(struct __cilkrts_worker));

  // Here we create a COMPLETELY NEW WORKER. 
  //__cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker+1);
  fresh_worker = make_worker(old_w->g, -1, fresh_worker); // Using self = -1.

  // Label the fresh worker as a replacement. 
  fresh_worker->is_replacement = 1;


  // Initialize the new worker:
  //---------------------------------   

  fresh_worker->l->type = WORKER_FREE;
  make_worker_system(fresh_worker);
  signal_node_msg(fresh_worker->l->signal_node, 1); // Set 'run' field to 1

  fresh_worker->l->scheduler_stack = sysdep_make_tiny_stack(fresh_worker);
}

//note: the w passed in here must be the current tls worker!
void setup_replacement_stack_and_run(__cilkrts_worker *w)
{
    void *ctx[5]; // Jump buffer for __builtin_setjmp/longjmp.

    CILK_ASSERT(w->l->scheduler_stack);

    // It may be that this stack has been used before (i.e., the worker was
    // bound to a thread), and in principle, we could just jump back into
    // the runtime, but we'd have to keep around extra data to do that, and
    // there is no harm in starting over, here.

    // Move the stack pointer onto the scheduler stack.  The subsequent
    // call will move execution onto that stack.  We never return from
    // that call, and every time we longjmp_into_runtime() after this,
    // the w->l->env jump buffer will be populated.
    if (0 == __builtin_setjmp(ctx)) {
        ctx[2] = w->l->scheduler_stack; // replace the stack pointer.
        __builtin_longjmp(ctx, 1);
    } else {
        // We can't just pass the worker through as a parameter to
        // worker_user_scheduler because the generated code might try to
        // retrieve w using stack-relative addressing instead of bp-relative
        // addressing and would get a bogus value.
        worker_replacement_scheduler(); // noinline, does not return.
    }

    CILK_ASSERT(0); // Should never reach this point.
}

/*
 * Special scheduling entrypoint for a WORKER_USER.  Ensure a new stack has been
 * created and the stack pointer has been placed on it before entering
 * worker_user_scheduler().
 *
 * Call this function the first time a WORKER_USER has returned to a stolen
 * parent and cannot continue.  Every time after that, the worker can simply
 * longjmp() like any other worker.
 */

#if defined __ICC 
#pragma intel optimization_level 0
#else 
#pragma GCC push_options
#pragma GCC optimize ("O0")
#endif
NOINLINE
void worker_replacement_scheduler()
{
    __cilkrts_worker *w = __cilkrts_get_tls_worker();

    w->reducer_map = 0;

    STOP_INTERVAL(w, INTERVAL_IN_SCHEDULER);
    STOP_INTERVAL(w, INTERVAL_WORKING);

    // Enter the scheduling loop on the worker. This function will
    // never return
    __cilkrts_run_scheduler_with_exceptions(w);

    CILK_ASSERT(0);
}

#if defined __ICC 
#else 
#pragma GCC pop_options
#endif

  inline void
inherit_forwarding_array(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker)
{
  volatile  __cilkrts_forwarding_array *cur;
  int i = 0;
  volatile int capacity;

  BEGIN_WITH_FORWARDING_ARRAY_LOCK(old_w->forwarding_array) {

    //immediately update the forwarding array of the new worker to be the same 
    //pointer as that of the old worker
    fresh_worker->forwarding_array = old_w->forwarding_array;
    fresh_worker->worker_cache = old_w->worker_cache;


    /**
     * skip over each array pointer until we find somewhere
     * with an open space
     */
    while(1) {
      cur = old_w->forwarding_array->links[i];
      if(!cur) break;
      if(cur->leftmost_idx > 0) break;
      i++;
    }

    //ALLOCATE A NEW BLOCK IF NECESSARY
    //-------------------
    if_f(cur && cur->elems >= ARRAY_SIZE-1) { //then we must be at the end of the list and its full
      capacity = *cur->capacity;

      //increment the capacity and reallocate
      //if this cas succeeds, we are committed and actually allocate the new memory without question
      *cur->capacity = capacity+GROW_ARRAY_INCREMENT;

      __cilkrts_forwarding_array **links = cur->links;

      cur->links = calloc(*cur->capacity, (sizeof(__cilkrts_forwarding_array *)));
      memcpy(cur->links, links, capacity);

      //TODO: this should be a mmap call. grab a whole page and then slice and dice.
      //populate the new space with forwarding array structs
      for (i=*cur->capacity-1; i>= *cur->capacity-GROW_ARRAY_INCREMENT; i--) {
        __cilkrts_forwarding_array *newa = memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 

        newa->elems = 0;
        newa->lock  = 0;
        newa->capacity = cur->capacity;
        newa->links    = cur->links;
        memset(newa->ptrs, 0, ARRAY_SIZE);

        //assign the new struct to the list of available arrays
        cur->links[i] = newa;
      }

      //reset the current array to be the fresh array just recently mapped
      cur = cur->links[*cur->capacity-1];
    }
    //------------------

    //ASSIGN REPLACEMENT TO ARRAY SLOT
    //-----------------

    //scan from the back of the list of the array and find
    //a place for a worker. there should be one now.
    for (i=ARRAY_SIZE-1; i >= 0; i--)
      if (cur->ptrs[i] == NULL) break;
    

    //TODO: <SAFETY> for now. This shouldn't have to be a cas. 
    cur->ptrs[i] = fresh_worker;
    //if(! cas(&cur->ptrs[i], NULL, fresh_worker))
    //  __cilkrts_bug("somehow our index became full while adding to the forwarding array! aborting\n");

    //increment the element counter
    cur->elems++;

    //reset the leftmost index if this wasn't a fragmentation fill
    if(i < cur->leftmost_idx) cur->leftmost_idx = i;

    //-----------------

    //remember where we are stored in the array for easy removal later
  } END_WITH_FORWARDING_ARRAY_LOCK(old_w->forwarding_array);
}

__cilkrts_worker *get_replacement_worker(__cilkrts_worker *w)
{
  __cilkrts_worker *fresh_worker = NULL;
  dequeue(w->worker_cache, (ELEMENT_TYPE *) fresh_worker);
  if(!fresh_worker) {
    fresh_worker = __cilkrts_malloc(sizeof(__cilkrts_worker)); 
    setup_new_worker(w, fresh_worker);
  } else {
    CILK_ASSERT(fresh_worker);
  }

  return fresh_worker;
}

__cilkrts_forwarding_array *init_array()
{
  int i;
  __cilkrts_forwarding_array *newa = NULL;

  __cilkrts_forwarding_array **links = (__cilkrts_forwarding_array **) 
    memalign(CACHE_LINE, INITIAL_CAPACITY*sizeof(__cilkrts_forwarding_array *)); 

  memset(links, 0, INITIAL_CAPACITY*sizeof(__cilkrts_forwarding_array **));

  for(i=0; i < INITIAL_CAPACITY; i++) {
    newa = memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 
    newa->leftmost_idx = ARRAY_SIZE-1;
    newa->lock = 0;
    newa->elems = 0;
    newa->links = links;
    links[i]    = newa;

    if(i == 0) {
      newa->capacity = (int *) __cilkrts_malloc (sizeof(int));
      *newa->capacity = INITIAL_CAPACITY;
    } else {
      newa->capacity = links[0]->capacity;
    }

    memset(newa->ptrs, 0, ARRAY_SIZE*sizeof(__cilkrts_worker *));
  }

  __cilkrts_fence();
  return newa->links[0]; //return the first link
}

//forwarding array mutexes
//--------------------------------------------------------

void forwarding_array_lock(volatile __cilkrts_forwarding_array *arr)
{
  int count;
  const int maxspin = 1000; /* SWAG */

  if (!TRY_ACQUIRE(arr->lock)) {
    count = 0;
    do {
      do {
        __cilkrts_short_pause();

        if (++count >= maxspin) {
          /* let the OS reschedule every once in a while */
          __cilkrts_yield();
          count = 0;
        }
      } while (arr->lock != 0);
    } while (!TRY_ACQUIRE(arr->lock));
  }
}

int forwarding_array_trylock(volatile __cilkrts_forwarding_array *arr)
{
  if (TRY_ACQUIRE(arr->lock)) {
    return 1;
  } else {
    return 0;
  }
}

void forwarding_array_unlock(volatile __cilkrts_forwarding_array *arr)
{
  CILK_ASSERT(arr->lock == 1);
  RELEASE(arr->lock);
}

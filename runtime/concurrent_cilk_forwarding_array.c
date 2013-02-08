#include "concurrent_cilk_forwarding_array.h"
#include <string.h>
#include <stdlib.h>

/* CILK FUCTIONS CALLED
 *  CILK_ASSERT
 *  __cilkrts_cilkscreen_ignore_block
 *  make_worker
 *  make_worker_system
 *  sysdep_make_tiny_stack
 *  signal_node_message
 *  __cilkrts_fence
 *  __cilkrts_set_tls_worker
 Create a new worker. */  
void setup_new_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker, volatile __cilkrts_paused_stack* stk) 
{   
  CILK_ASSERT(old_w);
  CILK_ASSERT(fresh_worker);
  CILK_ASSERT(stk);

  // WARNING: FUTURE CILKSCREEN SUPPORT WOULD REQUIRE MORE WORK HERE:
  // __cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker + sizeof(struct __cilkrts_worker));

  // Here we create a COMPLETELY NEW WORKER. 
  __cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker+1);
  fresh_worker = (__cilkrts_worker *) make_worker(old_w->g, -1, fresh_worker); // Using self = -1.

  // Initialize the new worker:
  //---------------------------------   

  fresh_worker->l->type = WORKER_FREE;
  make_worker_system(fresh_worker);
  signal_node_msg(fresh_worker->l->signal_node, 1); // Set 'run' field to 1

  fresh_worker->l->scheduler_stack = sysdep_make_tiny_stack(fresh_worker);

  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker] Created scheduler stack for replacement: %p\n", fresh_worker->l->scheduler_stack);

  //---------------------------------

  // Label the fresh worker as a replacement. 
  fresh_worker->is_replacement = 1;
}

/** Remove a replacement worker from the stealing list. 
 * At some point in the future, we should possibly shrink forwarding arrays
 * if when we remove a worker there are still elements left. For now, we just leave
 * the array intact. 
 */
inline void remove_replacement_worker(__cilkrts_worker *w) 
{
  //these don't need to be a cas operation because invalid
  //state is acceptable. The only reader is steal_work() in
  //the scheduler and if it turns up with this worker, it shouldn't
  //be able to steal from it. If it gets null, it will also fail
  //the steal. Therefore since both states are valid states, there
  //is no need to pay for a cas.
  *w->array_loc = NULL;
   w->array_loc = NULL;
   while(!atomic_sub(&w->array_block->elems,1));
}

/** When adding a replacement worker, we don't always have to run setup new worker. This is the shortcut
 * That initializes the state for a worker that has previously had setup_new_worker() invoked on it. 
 * This would only be run in the case of a cache hit on our replacement worker cache.
 *
 * This function will take care of all the plumbing to add the replacment worker to the old blocked worker
 * and make sure that the paused stack and the blocked parent know where the replacement lives.
 *
 * TODO: should we add the blocked parent if it isn't already a replacment worker? on one hand, this worker
 * already lives in the global worker array. On the other, we previously kept track of blocked parents
 * in the linked list style. 
 *
 */
  inline void 
add_replacement_worker(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker, volatile __cilkrts_paused_stack *stk)
{
  //leaving this for now, but we really don't need this. It is only for debugging purposes
  stk->replacement_worker = fresh_worker;
  CILK_ASSERT(stk->replacement_worker);
  //end stuff we don't need

  //we inherit the paused stack of our parent
  fresh_worker->paused_but_ready_stacks = old_w->paused_but_ready_stacks;

  //pass down the forwarding array to the old worker's
  //replacement
  inherit_forwarding_array(old_w, fresh_worker);

  //increment the reference count
  CILK_ASSERT(fresh_worker->reference_count == 0);
  /**
   * this could use a CAS instruction here,
   * however at this point no one should even know
   * that this worker exists. Therefore a CAS would
   * be unecessary as no worker should be able to race
   * for this piece of memory. 
   */
  fresh_worker->reference_count++; 

  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker] Created REPLACEMENT worker %d/%p, paused stack %p\n", 
      fresh_worker->self, fresh_worker, stk);

  //At this point the current OS thread should forget about the old worker.
  __cilkrts_set_tls_worker(fresh_worker);
}

  inline void
inherit_forwarding_array(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker)
{
  //do we need a worker lock here?
  //BEGIN_WITH_WORKER_LOCK(old_w) {
  
    __cilkrts_forwarding_array *cur;
    uintptr_t *arr;
    int i;

    /**
    * the cur member of the struct contains the most recent array
    * that had a worker removed from it, and is thus the most likely
    * array to have an open space. If that current array was full
    * then we have to pay for a search of the linked list. This
    * involkes skipping over each elem field to see if it is worth
    * investigating the actualy array for an open slot
    */
    if(old_w->forwarding_array->cur->elems >= ARRAY_SIZE)
      cur = old_w->forwarding_array;
    else
      cur = old_w->forwarding_array->cur;

    /**
     * This label is here because we are effectively error catching. 
     * When a CAS operation fails it is because someone has already
     * replaced the pointer to the next array block with a valid one.
     */
failed_cas:

    /**
     * skip over each array pointer until we find somewhere
     * with an open space
     */
    while(cur->elems >= ARRAY_SIZE && (long) cur->ptrs[ARRAY_SIZE-1] != END_OF_LIST) 
      cur = cur->next;

    //update the cur pointer to an array that probably has space
    old_w->forwarding_array->cur = cur;

    //ALLOCATE A NEW BLOCK IF NECESSARY
    //-------------------
    if_f(cur->elems >= ARRAY_SIZE) { //then we must be at the end of the list and its full

      __cilkrts_forwarding_array *newa =  (__cilkrts_forwarding_array *)
        __cilkrts_malloc(sizeof(__cilkrts_forwarding_array));

      if_f(!cas(&cur->next,NULL,newa)){
        //looks like someone already was here and added on to the list.
        //free the array we just made and retry by jumping
        //backup up before the while loop.
        __cilkrts_free(newa);
        goto failed_cas;
      }
      newa->elems = 0; 
      old_w->forwarding_array->next = newa;
    }
    //------------------

    //ASSIGN REPLACEMENT TO ARRAY SLOT
    //-----------------
    do {

      //even now someone could have filled
      //up the buffer
      if_f(cur->elems >= ARRAY_SIZE) 
        goto failed_cas;

      for (i=0; i < ARRAY_SIZE; i++)
        if (!cur->ptrs[i]) break;
    } while(!cas(&cur->ptrs[i], NULL, fresh_worker));

    //remember where we are store in the array for easy removal
    //later
    fresh_worker->array_loc = &cur->ptrs[i];
    fresh_worker->array_block = cur;


    //increment the element counter
    while(!atomic_add(&cur->elems,1));
    //-----------------

    //} END_WITH_WORKER_LOCK(old_w);
}

__cilkrts_worker *get_replacement_worker(__cilkrts_worker *w, volatile __cilkrts_paused_stack *stk)
{
  __cilkrts_worker *fresh_worker = NULL;
#ifdef CILK_IVARS_CACHING
  if_f(dequeue(w->worker_cache, (ELEMENT_TYPE *) &fresh_worker)) {
    fresh_worker = (__cilkrts_worker*)__cilkrts_malloc(sizeof(__cilkrts_worker)); 
    fresh_worker->reference_count = 0;
    setup_new_worker(w, fresh_worker, stk);
  } else {
    IVAR_DBG_PRINT_(1,"[concurrent-cilk] got new CACHED worker %d/%p \n",fresh_worker->self, fresh_worker);
    CILK_ASSERT(w->cached == 1);
    CILK_ASSERT(w->reference_count == 0);
    w->cached = 0;
    //the worker was locked before being placed
    //in the cache. It is now available for use.
    __cilkrts_worker_unlock(fresh_worker);
  }
#else
  fresh_worker = (__cilkrts_worker *) __cilkrts_malloc(sizeof(__cilkrts_worker)); 
  setup_new_worker(w, fresh_worker, stk);
#endif

  return fresh_worker;
}

__cilkrts_forwarding_array *init_array()
{
  __cilkrts_forwarding_array *arr = __cilkrts_malloc(sizeof(__cilkrts_forwarding_array));
  arr->elems = 0;
  arr->prev = NULL;
  arr->next = NULL;
  arr->cur  = arr;
  arr->head = arr;
  bzero(arr->ptrs,ARRAY_SIZE);
  
  return arr;
}

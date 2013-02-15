#include "concurrent_cilk_forwarding_array.h"
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

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
  
  *w->array_loc   = NULL; //null out the the slot in the forwarding array
   w->array_loc   = NULL; //null out the worker's reference to the slot
   while(!atomic_sub(&w->array_block->elems,1)) spin_pause();
   w->array_block = NULL;
   __cilkrts_fence();
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
    __cilkrts_forwarding_array *cur;
    uintptr_t *arr;
    int i = 0, j = 0;
    uint32_t nblocks;
    volatile int capacity;

    //immediately update th forwarding array of the new worker to be the same 
    //pointer as that of the old worker
    fresh_worker->forwarding_array = old_w->forwarding_array;
    IVAR_DBG_PRINT_(1,"[forwarding_array, inherit_forwarding_array] fresh worker %d/%p inherited forwarding_array %p from old worker: %d/%p\n",
        fresh_worker->self, fresh_worker, old_w->forwarding_array, old_w->self, old_w);

    /**
     * This label is here because we are effectively error catching. 
     * When a CAS operation fails it is because someone has already
     * replaced the pointer to the next array block with a valid one.
     */
failed_cas:

    /**
     * take a snapshot of how large our capacity is before we try
     * to find a place with a free space.
     */
    capacity = *old_w->forwarding_array->capacity;
    CILK_ASSERT(capacity > 0);

    /**
     * skip over each array pointer until we find somewhere
     * with an open space
     */
    for(i=capacity-1; i>=0; i--) {

      cur = old_w->forwarding_array->links[i];
      if(cur->elems < ARRAY_SIZE-1) break;
    }

    //ALLOCATE A NEW BLOCK IF NECESSARY
    //-------------------
    if_f(cur->elems >= ARRAY_SIZE-1) { //then we must be at the end of the list and its full
      IVAR_DBG_PRINT_(1,"RAN OUT OF SPACE IN FORWARDING ARRAY. Allocating some more. Capacity: %d vs. %d\n", capacity, *old_w->forwarding_array->capacity);

      //increment the capacity and reallocate
      //if this cas succeeds, we are committed and actually allocate the new memory without question
      if_t(cas(cur->capacity,capacity, capacity+GROW_ARRAY_INCREMENT)) { 

        __cilkrts_forwarding_array **links = cur->links;

        //BIG WHOPPING TODO: (I.E. YOU WILL CERTAINLY SEGFAULT)
        //the new realloced memory needs to be set to NULL, otherwise
        //you will grab an invalid pointer
        cur->links = (__cilkrts_forwarding_array **) 
          realloc(cur->links, capacity*(sizeof(__cilkrts_forwarding_array *)));

        //if realloc returned a brand new pointer, we need to flush out this cache line
        if(links != cur->links) {
          clear_cache(&cur->links, (&cur->links)+CACHE_LINE);
        }

        //populate the new space with forwarding array structs
        for (i=*cur->capacity-1; i>= *cur->capacity-GROW_ARRAY_INCREMENT; i--) {
          __cilkrts_forwarding_array *newa =  (__cilkrts_forwarding_array *)
            memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 

          newa->elems    = 0;
          newa->capacity = cur->capacity;
          newa->links    = cur->links;
          bzero(newa->ptrs, ARRAY_SIZE);

          //assign the new struct to the list of available arrays
          cur->links[i] = newa;
        }

        //reset the current array to be the fresh array just recently mapped
        cur = cur->links[*cur->capacity-1];

        //now force all loads and stores above this to complete.
        __cilkrts_fence();
      } else goto failed_cas; //looks like someone already was here and added on to the list.
    }
    //------------------

    //ASSIGN REPLACEMENT TO ARRAY SLOT
    //-----------------
    do {

      //even now someone could have filled
      //up the buffer
      if_f(cur->elems >= ARRAY_SIZE) 
        goto failed_cas;

      //scan from the back of the list of the array and find
      //a place for a worker. there should be one now.
      for (i=ARRAY_SIZE-1; i >= 0; i--)
        if (cur->ptrs[i] == NULL) break;
    } while(!cas(&cur->ptrs[i], NULL, fresh_worker));


    //in the off chance that we had so many workers that the whole block got filled up
    //in the above for loop, retry again.
    if(cur->ptrs[i] != fresh_worker){
      IVAR_DBG_PRINT_(1,"FAILED CAS on worker assignemnt\n");
      goto failed_cas;
    } 

    //increment the element counter
    while(!atomic_add(&cur->elems,1)) spin_pause();
    //-----------------

    //remember where we are stored in the array for easy removal later
    fresh_worker->array_loc = &cur->ptrs[i];
    fresh_worker->array_block = cur;


}

__cilkrts_worker *get_replacement_worker(__cilkrts_worker *w, volatile __cilkrts_paused_stack *stk)
{
  __cilkrts_worker *fresh_worker = NULL;
#ifdef CILK_IVARS_CACHING
  if(dequeue(w->worker_cache, (ELEMENT_TYPE *) &fresh_worker)) {
    fresh_worker = (__cilkrts_worker*)__cilkrts_malloc(sizeof(__cilkrts_worker)); 
    fresh_worker->reference_count = 0;
    setup_new_worker(w, fresh_worker, stk);
  } else {
    IVAR_DBG_PRINT_(1,"[concurrent-cilk] got new CACHED worker %d/%p \n",fresh_worker->self, fresh_worker);
    CILK_ASSERT(fresh_worker->cached == 1);
    CILK_ASSERT(fresh_worker->reference_count == 0);
    fresh_worker->cached = 0;
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
  int i;
  __cilkrts_forwarding_array *newa = NULL;

  __cilkrts_forwarding_array **links = (__cilkrts_forwarding_array **) 
   memalign(CACHE_LINE, INITIAL_CAPACITY*sizeof(__cilkrts_forwarding_array *)); 

  bzero(links, INITIAL_CAPACITY*sizeof(__cilkrts_forwarding_array **));

  for(i=0; i < INITIAL_CAPACITY; i++) {
    newa = (__cilkrts_forwarding_array *) memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 
    newa->elems = 0;
    newa->links = links;
    links[i]    = newa;

    //the first time, capacity gets a value.
    //the pointer is passed around every time after that
    if(i == 0) {
      newa->capacity = (int *) __cilkrts_malloc(sizeof(int));
      *(newa->capacity) = INITIAL_CAPACITY;
    } else 
      newa->capacity = links[0]->capacity;
    
    bzero(newa->ptrs, ARRAY_SIZE*sizeof(__cilkrts_worker *));
  }

  return newa->links[0]; //return the first link
}

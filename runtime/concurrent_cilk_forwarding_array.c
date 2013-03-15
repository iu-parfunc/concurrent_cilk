#include "concurrent_cilk_forwarding_array.h"
#include "scheduler.h"
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)
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
void setup_new_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker) 
{   
  CILK_ASSERT(old_w);
  CILK_ASSERT(fresh_worker);

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

  //move up the lefmost index if this is the leftmost pointer
  if(*w->array_loc == w->array_block->ptrs[w->array_block->leftmost_idx]) 
    w->array_block->leftmost_idx++;

  w->array_block->elems--;
  *w->array_loc   = NULL; //null out the the slot in the forwarding array
  w->array_loc   = NULL; //null out the worker's reference to the slot
  w->array_block = NULL;
}

/** When adding a replacement worker, we don't always have to run setup new worker. This is the shortcut
 * That initializes the state for a worker that has previously had setup_new_worker() invoked on it. 
 * This would only be run in the case of a cache hit on our replacement worker cache.
 *
 * This function will take care of all the plumbing to add the replacment worker to the old blocked worker
 * and make sure that the paused stack and the blocked parent know where the replacement lives.
 *
 * TODO: should we add the blocked parent if it isn't already a replacement worker? on one hand, this worker
 * already lives in the global worker array. On the other, we previously kept track of blocked parents
 * in the linked list style. 
 *
 */
  inline void 
add_replacement_worker(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker, volatile __cilkrts_paused_stack *stk)
{
  stk->replacement_worker = fresh_worker;

  //printf("fresh worker: %d/%p stack: %p\n", stk->replacement_worker->self, stk->replacement_worker, stk);
  //pass down the forwarding array to the old worker's
  //replacement
  inherit_forwarding_array(old_w, fresh_worker);
}

  inline void
inherit_forwarding_array(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker)
{
  int i = 0;
  volatile int capacity;
  __cilkrts_forwarding_array *cur;

  //immediately update the forwarding array of the new worker to be the same 
  //pointer as that of the old worker
  fresh_worker->forwarding_array = old_w->forwarding_array;

  capacity = *old_w->forwarding_array->capacity;

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
  if_f(cur->elems >= ARRAY_SIZE-1) { //then we must be at the end of the list and its full

    //increment the capacity and reallocate
    //if this cas succeeds, we are committed and actually allocate the new memory without question
    *cur->capacity = capacity+GROW_ARRAY_INCREMENT;

    __cilkrts_forwarding_array **links = cur->links;

    cur->links = (__cilkrts_forwarding_array **) 
      realloc(cur->links, capacity*(sizeof(__cilkrts_forwarding_array *)));

    //populate the new space with forwarding array structs
    for (i=*cur->capacity-1; i>= *cur->capacity-GROW_ARRAY_INCREMENT; i--) {
      __cilkrts_forwarding_array *newa = memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 

      newa->elems    = 0;
      newa->capacity = cur->capacity;
      newa->links    = cur->links;
      bzero(newa->ptrs, ARRAY_SIZE);

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
  cur->ptrs[i] = fresh_worker;

  //reset the leftmost index if this wasn't a fragmentation fill
  if(i < cur->leftmost_idx) cur->leftmost_idx = i;

  //increment the element counter
  cur->elems++;
  //-----------------
  //set the leftmost pointer in the array block is saved.
  //this represent our "best guess" as to where the array
  //has been populated to. 
  cur->leftmost_idx = i;

  //remember where we are stored in the array for easy removal later
  fresh_worker->array_loc = &cur->ptrs[i];
  fresh_worker->array_block = cur;
}

__cilkrts_worker *get_replacement_worker(__cilkrts_worker *w)
{
  __cilkrts_worker *fresh_worker = NULL;
  dequeue(w->worker_cache, (ELEMENT_TYPE *) &fresh_worker);
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

  bzero(links, INITIAL_CAPACITY*sizeof(__cilkrts_forwarding_array **));

  for(i=0; i < INITIAL_CAPACITY; i++) {
    newa = memalign(CACHE_LINE, sizeof(__cilkrts_forwarding_array)); 
    newa->leftmost_idx = ARRAY_SIZE-1;
    newa->elems = 0;
    newa->links = links;
    links[i]    = newa;

    if(i == 0) {
      newa->capacity = (int *) malloc (sizeof(int));
      *newa->capacity = INITIAL_CAPACITY;
    } else {
      newa->capacity = links[0]->capacity;
    }

    bzero(newa->ptrs, ARRAY_SIZE*sizeof(__cilkrts_worker *));
  }

  return newa->links[0]; //return the first link
}

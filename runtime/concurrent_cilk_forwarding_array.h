#ifndef __CONCURRENT_CILK_FORWARDING_ARRAY_H
#define __CONCURRENT_CILK_FORWARDING_ARRAY_H

__CILKRTS_BEGIN_EXTERN_C

typedef struct __cilkrts_forwarding_array __cilkrts_forwarding_array;
typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;

///TODO: add some runtime knobs to tweak this
#define ARRAY_SIZE 1024
#define CACHE_LINE 64
#define GROW_ARRAY_INCREMENT 4
#define INITIAL_CAPACITY 1
#include "concurrent_cilk_internal.h"
#define align(n) __attribute__((aligned(n)))

///end of the worker list should not be in the available range for malloc.
#define END_OF_LIST 1

/* m->lock == 1 means that mutex M is locked */
#define TRY_ACQUIRE(lock) (__cilkrts_xchg(&lock, 1) == 0)

/* ICC 11.1+ understands release semantics and generates an
 *    ordinary store with a software memory barrier. */
#if __ICC >= 1110
#define RELEASE(lock) __sync_lock_release(&lock)
#else
#define RELEASE(lock) __cilkrts_xchg(&lock, 0)
#endif

#define BEGIN_WITH_FORWARDING_ARRAY_LOCK(arr) forwarding_array_lock(arr); do
#define END_WITH_FORWARDING_ARRAY_LOCK(arr)  while (forwarding_array_unlock(arr), 0)

/** The forwarding array keeps track of forwarding pointers
 * that will be stolen from if the parent worker runs out of work.
 * This strategy effectively is a dynamic expansion of the way that
 * normal cilk does a steal by selecting a random victim. In our case
 * however, the victim is dynamically created as children and parents
 * of blocked workers. 
 *
 * The creation and deletion of a forwarding pointer in an array is as follows:
 *
 * 1. upon a __cilkrts_finalize_pause() call, a replacement worker is obtained
 *    from either the worker cache or is malloced. At this point the replacement
 *    worker has 0 references. 
 *
 * 2. a) increment the reference count on the replacment worker since it is running,
 *       in place of the blocked worker. 
 *
 * 3. upon the parent worker becoming unblocked, decrement the reference count of its child.
 *
 * 4. if the reference count of a replacement worker is 0 
 *    and the worker returns to the scheduler, instead of a steal it is returned
 *    to the worker cache or free()d.  The victim selected (not the same worker) 
 *    is set as the current tls worker and the steal is failed.
 *    The recently selected victim returns to the scheduler. If it has work,
 *    it will run it. If there is no work, it will run and get cached in the same way.
 *
 * 5. non replacement workers are never cached. A blocked non replacement worker is restored
 *    when check_concurrent_work() runs in the scheduler and that worker's paused stack is dequeued.
 *    Immediately the blocked worker is restored and the replacement worker yields the computation. 
 *    That replacement worker will only get cached when it is a victim of a steal. 
 *    Because of this situation, we modify the behavior of steals on replacement workers
 *    to a worker swap and fail of the steal even if the replacement worker has no actual work.
 *    In reality, there is actually work to be done, but that work is maintenance work on the rts
 *    state by returning workers to the cache or free()ing them.
 */
struct __cilkrts_forwarding_array {

  ///these three elements are accessed in an add of an element
  ///so we keep them all on one cache line.
  volatile int leftmost_idx;
  volatile int elems;                
  volatile int *capacity;
  volatile int lock;

  ///there is contention over the elements of the array
  ///each operation must be a cas
  __cilkrts_worker *ptrs[ARRAY_SIZE] align(64);

  struct __cilkrts_forwarding_array **links align(64);

} align(64);

void inherit_forwarding_array(__cilkrts_worker *old_w, __cilkrts_worker *fresh_worker);
void setup_new_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker);
void remove_replacement_worker(__cilkrts_worker *w);
void destroy_array(__cilkrts_worker *w);
__cilkrts_worker *get_replacement_worker(__cilkrts_worker *w);
__cilkrts_forwarding_array *init_array();

void forwarding_array_unlock(volatile __cilkrts_forwarding_array *arr);
int forwarding_array_trylock(volatile __cilkrts_forwarding_array *arr);
void forwarding_array_lock(volatile __cilkrts_forwarding_array *arr);

void setup_replacement_stack_and_run(__cilkrts_worker *w);
NOINLINE void worker_replacement_scheduler();

__CILKRTS_END_EXTERN_C
#endif


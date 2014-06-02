// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.
#include <stdio.h>
#include "scheduler.h"
#include "concurrent_cilk_internal.h"
#include "cilk_malloc.h"
#include "sysdep.h"
#include "signal_node.h"
#include "local_state.h"
#include "concurrent_queue.h"

CILK_API(void)
__cilkrts_commit_pause(__cilkrts_paused_fiber *pfiber) 
{
  __cilkrts_worker *w; 
  __cilkrts_worker *replacement_worker;
  
  //initialize the new worker state and save the current worker state in the paused fiber's context.
  pfiber->w = w = __cilkrts_get_tls_worker_fast(); 
  replacement_worker = make_worker(w->g, w->self, __cilkrts_malloc(sizeof(__cilkrts_worker)));
  //make replacement a system worker
  replacement_worker->l->type = WORKER_SYSTEM;
  replacement_worker->l->signal_node = signal_node_create();

  //no lock needed. Each worker only edits its own slot.
  //"push" the replacement worker on the top of the stealing stack.
  w->g->workers[w->self] = replacement_worker;

  //sets pthread TLS to replacement worker and invokes the scheduler.
  __cilkrts_worker_stub((void *)replacement_worker);
}

CILK_API(void)
  __cilkrts_resume_fiber(__cilkrts_paused_fiber *pfiber)
{
  __cilkrts_worker *old_w = __cilkrts_get_tls_worker_fast();
  CILK_ASSERT(old_w); 
  CILK_ASSERT(old_w->self == pfiber->w->self);

  //"pop" the replacement worker off the stealing stack
  old_w->g->workers[old_w->self] = pfiber->w;
  longjmp(pfiber->ctx, (uintptr_t)  old_w);
  CILK_ASSERT(0); //no return
}

int paused_fiber_trylock(__cilkrts_paused_fiber *pfiber) {
  return cas(&pfiber->lock, 0, 1);
}

void paused_fiber_unlock(__cilkrts_paused_fiber *pfiber) {
  atomic_release(&pfiber->lock, 0);
}

void __cilkrts_cleanup_replacement_worker(__cilkrts_worker *w)
{
  //TODO
}


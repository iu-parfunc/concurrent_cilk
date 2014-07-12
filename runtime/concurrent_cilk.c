//  Concurrent Cilk: 
//    An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
//  concurrency.
#include <stdio.h>
#include "scheduler.h"
#include "concurrent_cilk_internal.h"
#include "cilk_malloc.h"
#include "sysdep.h"
#include "signal_node.h"
#include "local_state.h"
#include "concurrent_queue.h"



CILK_API(__cilkrts_worker *)
__cilkrts_commit_pause(__cilkrts_worker *w) 
{
  __cilkrts_worker *replacement;
  
  // initialize the new worker state and save the current worker state in the paused fiber's context.
  replacement = make_worker(w->g, w->self, __cilkrts_malloc(sizeof(__cilkrts_worker)));

  // make replacement a system worker and fill in record keeping for replacement workers.
  replacement->l->type        = WORKER_SYSTEM;
  replacement->l->signal_node = signal_node_create();
  signal_node_msg(replacement->l->signal_node, 1); // set status to run.

  // no lock needed. Each worker only edits its own slot.
  // "push" the replacement worker on the top of the stealing stack.
  w->g->workers[w->self] = replacement;

  //lazily allocate a fibers array.
  if (! w->fibers) {
    w->fibers = (__cilkrts_worker **) __cilkrts_malloc(sizeof(__cilkrts_worker *) * MAX_WORKERS_BLOCKED);
    memset(w->fibers, 0, sizeof(__cilkrts_worker *) * MAX_WORKERS_BLOCKED);
  }

  //lazily allocate a slot for the readylist.
  if (! w->readylist) { w->readylist = make_stack_queue(); }

  //lazily allocate a pointer for the ref_count. 
  if (! w->ref_count) { w->ref_count = __cilkrts_malloc(sizeof(int)); *w->ref_count = 0; }
  if (0 == *w->ref_count) {
    atomic_add(&(w->g->workers_blocked), 1);
  }

  //the fibers pointer is shared across all replacements on the same thread,
  //but only this thread may write to the array.
  replacement->fibers       = w->fibers;
  replacement->readylist    = w->readylist;
  replacement->worker_depth = w->worker_depth+1;
  replacement->ref_count    = w->ref_count;
  *replacement->ref_count += 1;

  return replacement;
  // make sure you call the scheduler!
}


CILK_API(void)
  __cilkrts_resume_fiber(__cilkrts_worker *w)
{

  __cilkrts_worker *old_w = __cilkrts_get_tls_worker_fast();
  CILK_ASSERT(w);
  CILK_ASSERT(old_w); 
  CILK_ASSERT(!can_steal_from(old_w));
  CILK_ASSERT(w->self == old_w->self);

  dbgprint(CONCURRENT, "restoring worker %p current worker %p depth %i sf %p\n", w, old_w, w->worker_depth, w->current_stack_frame);
  remove_worker_from_stealing(w);
  w->g->workers[w->self] = w;
  __cilkrts_set_tls_worker(w);
  if (1 == *w->ref_count) {
    atomic_sub(&(w->g->workers_blocked), 1);
  }
  *w->ref_count -= 1;
  longjmp((struct __jmp_buf_tag *) w->paused_ctx, 1);
  CILK_ASSERT(0); // no return
}

void __cilkrts_cleanup_replacement_worker(__cilkrts_worker *w)
{
  // TODO
  //  -- check the ready queue is empty
  //  -- NULL the parent pointer
  //  -- ...?
  //  -- free/cache worker struct
}

void register_worker_for_stealing(__cilkrts_worker *w) 
{
  int i;
  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) {
    if (w->fibers[i] == NULL) {
      dbgprint(CONCURRENT, "registered worker %d/%p for stealing at idx %i\n", w->self, w, i);
      w->fibers[i] = w;
      return;
    }
  }

  if (i >= MAX_WORKERS_BLOCKED) { __cilkrts_bug("BLOCKED WORKER OVERFLOW - aborting"); }
}

inline void is_last_paused_worker(__cilkrts_worker *w)
{
  CILK_ASSERT(w);

}

inline void remove_worker_from_stealing(__cilkrts_worker *w)
{
  int i;
  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) { 
    if (w->fibers[i] == w) {
      dbgprint(CONCURRENT, "removed worker %d/%p from stealing at idx %i\n", w->self, w, i);
      w->fibers[i] = NULL;
      return;
    }
  }
}

inline __cilkrts_worker *find_replacement_worker(__cilkrts_worker *w)
{
  int i; 
  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) {
    if (w->fibers[i] != NULL) {
      return w->fibers[i];
    }
  } 
  return NULL;
}


/**
 * There is actually a data race between find_concurrent_work and find_ready_fiber. 
 * find_concurrent_work is executed by another worker while searching for a stealing
 * victim. find_ready_fiber is executed by a thread on its own TLS to find a ready_fiber
 * on its own thread to restore. Only one writes, so this is ok, but it does change the
 * order on a given run which a steal might be found. 
 */
inline __cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim)
{
  int i;
  CILK_ASSERT(victim);

  // there may not be any concurrent work to steal
  if (! victim->fibers) { return victim; }

  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) {
    if(victim->fibers[i] && can_steal_from(victim->fibers[i])) {
      dbgprint(CONCURRENT, "found victim %d/%p for stealing at idx %i\n",
          victim->fibers[i]->self, victim->fibers[i], i);
      return victim->fibers[i];
    }
  }
  return victim;
}


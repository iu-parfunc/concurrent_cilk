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



/**
 * Pauses a worker and returns a new worker ready to be used as a replacement. 
 *
 * @param __cilkrts_worker *w the worker to pause
 * @return A New  __cilkrts_worker * set up to be run while the current worker is blocked. 
 */
CILK_API(__cilkrts_worker *)
__cilkrts_commit_pause(__cilkrts_worker *w) 
{
  __cilkrts_worker * volatile* fibers = NULL;
  dbgprint(CONCURRENT, "COMMIT PAUSE blocked worker %d/%p team %s\n",
      w->self, w, w->l->type == WORKER_SYSTEM ? "WORKER_SYSTEM" : "WORKER_USER");
  __cilkrts_worker *replacement;
  
  // initialize the new worker state and save the current worker state in the paused fiber's context.
  replacement = make_worker(w->g, w->self, __cilkrts_malloc(sizeof(__cilkrts_worker)));


  //lazily allocate a fibers array.
  if (! w->fibers) {
    fibers = (__cilkrts_worker * volatile*)
      __cilkrts_malloc(sizeof(__cilkrts_worker *) * MAX_WORKERS_BLOCKED);
    memset((__cilkrts_worker **) fibers, 0, sizeof(__cilkrts_worker *) * MAX_WORKERS_BLOCKED);
    w->fibers = fibers;
  }

  //lazily allocate a slot for the readylist.
  if (! w->readylist) { w->readylist = make_stack_queue(); }

  //lazily allocate a slot for the referencelist.
  if (! w->referencelist) { w->referencelist = make_stack_queue(); }

  //lazily allocate a pointer for the ref_count. 
  if (! w->ref_count) {
    w->ref_count = __cilkrts_malloc(sizeof(int)); *w->ref_count = 0;
  }

  if (0 == *w->ref_count) {
    atomic_add(&(w->g->workers_blocked), 1);
  }

  w->blocked = 1;

  CILK_ASSERT(NULL != w->l->team->team_leader);

  //the fibers pointer is shared across all replacements on the same thread,
  //but only this thread may write to the array.
  replacement->fibers        = w->fibers;
  replacement->readylist     = w->readylist;
  replacement->ref_count     = w->ref_count;
  replacement->team_leader   = w->team_leader;
  replacement->l->team       = w->l->team;
  replacement->l->type       = w->l->type;
  replacement->worker_depth  = w->worker_depth+1;
  replacement->referencelist = w->referencelist;
  *replacement->ref_count += 1;

  if (WORKER_SYSTEM == replacement->l->type) {
    // make replacement a system worker and fill in record keeping for replacement workers.
    replacement->l->signal_node = signal_node_create();
    signal_node_msg(replacement->l->signal_node, 1); // set status to run.
  }

  dbgprint(CONCURRENT, "CREATED replacement worker %d/%p\n", replacement->self, replacement);
  // no lock needed. Each worker only edits its own slot.
  // "push" the replacement worker on the top of the stealing stack.
  w->g->workers[w->self] = replacement;
  return replacement;
  // make sure you call the scheduler!
}

/**
 * Rollback a pause which has been committed (via __cilkrts_commit_pause()),
 * but the calling thread has not yet gone to the cilk scheduler to steal work. 
 *
 * @param blocked_w The worker which is currently blocked
 * @param replacement_w The worker which replaced blocked_w
 */
CILK_API(void)
__cilkrts_rollback_pause(__cilkrts_worker *blocked_w, __cilkrts_worker *replacement_w)
{
  blocked_w->blocked = 0;
  //We must obtain the lock on the replacement worker to keep thieves away
  BEGIN_WITH_WORKER_LOCK(replacement_w) {
    blocked_w->g->workers[replacement_w->self] = blocked_w;
  } END_WITH_WORKER_LOCK(replacement_w);
  __cilkrts_fence();
  __cilkrts_set_tls_worker(blocked_w);
  //TODO: cache
  __cilkrts_free(replacement_w);
}


CILK_API(void)
  __cilkrts_resume_fiber(__cilkrts_worker *w)
{

  __cilkrts_worker *old_w = __cilkrts_get_tls_worker_fast();
  CILK_ASSERT(w);
  CILK_ASSERT(old_w); 
  dbgprint(CONCURRENT, "restoring worker %d/%p/%i REAPING old worker %d/%p/%i\n",
      w->self, w, w->worker_depth, old_w->self, old_w, old_w->worker_depth);
  CILK_ASSERT(!can_steal_from(old_w));
  CILK_ASSERT(!old_w->l->next_frame_ff);
  CILK_ASSERT(w->self == old_w->self);
  CILK_ASSERT(old_w != w);

  remove_worker_from_stealing(w);
  w->g->workers[w->self] = w;
  w->blocked = 0;
  __cilkrts_set_tls_worker(w);
  if (1 == *w->ref_count) {
    atomic_sub(&(w->g->workers_blocked), 1);
  }
  *w->ref_count -= 1;
  longjmp((struct __jmp_buf_tag *) w->paused_ctx, 1);
  CILK_ASSERT(0); // no return
}

CILK_API(void) 
  __cilkrts_run_replacement_fiber(__cilkrts_worker *replacement)
{
  //sets pthread TLS to replacement worker and invokes the scheduler.
  __cilkrts_worker_stub((void *) replacement);
}

void __cilkrts_cleanup_replacement_worker(__cilkrts_worker *w)
{
  // TODO
  //  -- check the ready queue is empty
  //  -- NULL the parent pointer
  //  -- ...?
  //  -- free/cache worker struct
}

CILK_API(void)
__cilkrts_register_blocked_worker_for_stealing(__cilkrts_worker *w) 
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

void remove_worker_from_stealing(__cilkrts_worker *w)
{
  int i;
  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) { 
    if (w->fibers[i] == w) {
      w->fibers[i] = NULL;
      dbgprint(CONCURRENT, "removed worker %d/%p from stealing at idx %i\n", w->self, w, i);
      return;
    }
  }
}

inline __cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim)
{
  int i;
  __cilkrts_worker *surrogate = victim;
  __cilkrts_worker volatile *tmp;
  CILK_ASSERT(victim);

  // there may not be any concurrent work to steal
  if (! victim->fibers) { return victim; }

  for (i = 0; i < MAX_WORKERS_BLOCKED; i++) {
    tmp = victim->fibers[i];
    if(tmp) {
      if (can_steal_from((__cilkrts_worker *) tmp)) {
        surrogate = (__cilkrts_worker *) tmp;
        dbgprint(CONCURRENT, "victim %d/%p found surrogate victim %d/%p for stealing at idx %i\n",
            victim->self, victim, surrogate->self, surrogate, i);
        break;
      }
    }
  }
  return surrogate;
}




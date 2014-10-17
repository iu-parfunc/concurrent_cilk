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


inline CILK_API(int) __cilkrts_pause_fiber(jmp_buf *ctx)
{
  CILK_ASSERT(ctx);
  return setjmp((struct __jmp_buf_tag *) ctx);
}


CILK_API(__cilkrts_worker *)
__cilkrts_commit_pause(__cilkrts_worker *w, jmp_buf *ctx) 
{
  __cilkrts_worker *replacement;

  dbgprint(CONCURRENT, "COMMIT PAUSE paused worker %d/%p team %s\n",
      w->self, w, w->l->type == WORKER_SYSTEM ? "WORKER_SYSTEM" : "WORKER_USER");

  CILK_ASSERT(w->current_stack_frame);

  // the paused context of the current worker is the context which we called via __cilkrts_pause_fiber
  w->paused_ctx  = ctx;
  // initialize the new worker state and save the current worker state in the paused fiber's context.
  replacement = make_worker(w->g, w->self, __cilkrts_malloc(sizeof(__cilkrts_worker)));

  //lazily allocate a slot for the readylist.
  if (! w->readylist) { w->readylist = make_stack_queue(); }

  //lazily allocate a slot for the pauselist.
  if (! w->pauselist) { w->pauselist = make_stack_queue(); }

  //lazily allocate a pointer for the ref_count. 
  if (! w->ref_count) {
    w->ref_count = __cilkrts_malloc(sizeof(int)); *w->ref_count = 0;
  }

  //lazily allocate a pointer for the paused_event_accumulator. 
  if (! w->paused_event_accumulator) {
    w->paused_event_accumulator = __cilkrts_malloc(sizeof(int)); *w->paused_event_accumulator = 0;
  }

  if (0 == *w->ref_count) {
    atomic_add(&(w->g->workers_blocked), 1);
  }

  w->blocked = 1;

  CILK_ASSERT(NULL != w->l->team->team_leader);

  replacement->readylist     = w->readylist;
  replacement->ref_count     = w->ref_count;
  replacement->team_leader   = w->team_leader;
  replacement->l->team       = w->l->team;
  replacement->l->type       = w->l->type;
  replacement->worker_depth  = w->worker_depth+1;
  replacement->pauselist     = w->pauselist;
  replacement->paused_event_accumulator = w->paused_event_accumulator;
  *replacement->ref_count += 1;
  *replacement->paused_event_accumulator +=1;

  if (WORKER_SYSTEM == replacement->l->type) {
    // make replacement a system worker and fill in record keeping for replacement workers.
    replacement->l->signal_node = signal_node_create();
    signal_node_msg(replacement->l->signal_node, 1); // set status to run.
  }

  dbgprint(CONCURRENT, "CREATED replacement worker %d/%p\n", replacement->self, replacement);

  // The replacement now becomes the thread local state. 
  __cilkrts_set_tls_worker(replacement);

  // "push" the replacement worker on the top of the stealing stack.
  // runtime now forgets about w. 
  BEGIN_WITH_WORKER_LOCK(w) {
    w->g->workers[w->self] = replacement;
  } END_WITH_WORKER_LOCK(w);

  return replacement;
  // make sure you call the scheduler (or other user code with manual resume)!
}

// The old paused worker now becomes the thread local state. 
  CILK_API(void)
inline __cilkrts_roll_back_pause(__cilkrts_worker *paused_w, __cilkrts_worker *replacement_w)
{
  paused_w->paused_ctx = NULL;
  paused_w->blocked    = 0;

  // The paused worker is now reinstated at the thread local worker
  __cilkrts_set_tls_worker(paused_w);

  //We must obtain the lock on the replacement worker to keep thieves away
  BEGIN_WITH_WORKER_LOCK(replacement_w) {
    paused_w->g->workers[replacement_w->self] = paused_w;
  } END_WITH_WORKER_LOCK(replacement_w);
  __cilkrts_fence();
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

  __cilkrts_remove_paused_worker_from_stealing(w);
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

inline  CILK_API(void) 
__cilkrts_run_replacement_fiber(__cilkrts_worker *replacement)
{
  //sets pthread TLS to replacement worker and invokes the scheduler
  __cilkrts_worker_stub((void *) replacement);
}

inline CILK_API(void)
__cilkrts_register_paused_worker_for_stealing(__cilkrts_worker *w) 
{
  w->to_remove_from_stealing = 0;
  enqueue(w->pauselist, (ELEMENT_TYPE) w);
}

  inline  CILK_API(void)
__cilkrts_remove_paused_worker_from_stealing(__cilkrts_worker *w)
{
  w->to_remove_from_stealing = 1;
}


inline __cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim)
{
  __cilkrts_worker *surrogate = NULL;
  CILK_ASSERT(victim);

  // Proof of eventual progress: 
  //  * we exit when the pause list is empty
  //  * we round robin through the pause-list
  //  * any that have nothing to steal are removed from the pauselist,
  //    which gets monotonically shorter, UNLESS
  //  * the victim thread concurrently adds to the pauselist,
  //    which it cannot do an infinite number of times
  while (victim->pauselist) {
    if ((! dequeue(victim->pauselist, (ELEMENT_TYPE *) &surrogate)) && surrogate) {
      // Lazily remove any workers marked for deletion from the stealing queue
      if (surrogate->to_remove_from_stealing) { 
        surrogate->to_remove_from_stealing = 0;
        surrogate = NULL;
        continue;
      }

      // if we cannot steal, the worker is dequeued and not added back for
      // consideration. By definition a blocked worker cannot expose more work. 
      if (can_steal_from(surrogate)) {
        // We must put the worker back. There may be more work to steal from it later.
        enqueue(victim->pauselist, (ELEMENT_TYPE) surrogate);
        break;
      } 
    } else {
      break;
    }
  }
  return surrogate ? surrogate : victim;
}



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

//TMP
int len_parent_list(__cilkrts_worker *w)
{
  int len = 0;

  while(w) {
    ++len;
    w = w->parent;
  }
  return len;
}

CILK_API(void)
__cilkrts_commit_pause(__cilkrts_paused_fiber *pfiber) 
{
  __cilkrts_worker *w; 
  __cilkrts_worker *replacement;
  
  // initialize the new worker state and save the current worker state in the paused fiber's context.
  pfiber->paused = w = __cilkrts_get_tls_worker_fast(); 
  replacement = make_worker(w->g, w->self, __cilkrts_malloc(sizeof(__cilkrts_worker)));

  // make replacement a system worker and fill in record keeping for replacement workers.
  replacement->parent         = w;
  replacement->l->type        = WORKER_SYSTEM;
  replacement->l->signal_node = signal_node_create();
  signal_node_msg(replacement->l->signal_node, 1); // set status to run.

  // no lock needed. Each worker only edits its own slot.
  // "push" the replacement worker on the top of the stealing stack.
  pfiber->replacement    = replacement;
  w->g->workers[w->self] = replacement;
  //printf("COMMIT: pfiber %p, replacement %p\n", pfiber, pfiber->replacement);
  // make sure you call the scheduler!
}



CILK_API(void)
  __cilkrts_resume_fiber(__cilkrts_paused_fiber *pfiber)
{
  __cilkrts_worker *old_w = __cilkrts_get_tls_worker_fast();
  //printf("RESUME: pfiber %p, old_w %p replacement %p\n", pfiber, old_w, pfiber->replacement);
  CILK_ASSERT(old_w); 
  CILK_ASSERT(old_w == pfiber->replacement);
  CILK_ASSERT(old_w->self == pfiber->paused->self);
  CILK_ASSERT(!can_steal_from(old_w));

  // "pop" the replacement worker off the stealing stack
  __cilkrts_set_tls_worker(pfiber->paused);
  old_w->g->workers[old_w->self] = pfiber->paused;
  longjmp(pfiber->ctx, 1);
  CILK_ASSERT(0); // no return
}

int paused_fiber_trylock(__cilkrts_paused_fiber *pfiber) {
  return cas(&pfiber->lock, 0, 1);
}

void paused_fiber_unlock(__cilkrts_paused_fiber *pfiber) {
  atomic_release(&pfiber->lock, 0);
}

void __cilkrts_cleanup_replacement_worker(__cilkrts_worker *w)
{
  // TODO
  //  -- check the ready queue is empty
  //  -- NULL the parent pointer
  //  -- ...?
  //  -- free/cache worker struct
}


/**
 * There is actually a data race between find_concurrent_work and find_ready_fiber. 
 * find_concurrent_work is executed by another worker while searching for a stealing
 * victim. find_ready_fiber is executed by a thread on its own TLS to find a ready_fiber
 * on its own thread to restore. Only one writes, so this is ok, but it does change the
 * order on a given run which a steal might be found. 
 */
__cilkrts_worker *find_concurrent_work(__cilkrts_worker *victim)
{
  CILK_ASSERT(victim);
  __cilkrts_worker *w = victim;
  while(w) {
    if (can_steal_from(w)) { return w; }
    CILK_ASSERT(w != w->parent);
    w = w->parent;
  }
  return victim;
}



__cilkrts_worker *find_ready_fiber(__cilkrts_worker *head)
{
  CILK_ASSERT(head);
  CILK_ASSERT(head == __cilkrts_get_tls_worker_fast());
  __cilkrts_worker *prev;
  __cilkrts_worker *w = head;

  while(w) {
    if (w->ready_fiber && !can_steal_from(w)) { 
      if(w != head) { 
        //remove the w from the list
        prev->parent = w->parent;
        //w becomes the new head of the list
        w->parent = head;
      } //else head is ready -- no pointer update necessary.
      return w;
    }
    prev = w;
    w = w->parent;
  }
  return NULL;
}

void traverse_parent_list(__cilkrts_worker *w)
{
  while(w) {
    printf("(%i)(%i)0x%p->", w->ready_fiber != NULL, can_steal_from(w), w);
    w = w->parent;
  }
  printf("\n");
}


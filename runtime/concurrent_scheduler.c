#include "scheduler.h"
#include "full_frame.h"
#include "os.h"
#include "stacks.h"
#include "sysdep.h"
#include "sync.h"
#include "cilk-ittnotify.h"

//concurrent cilk conditional includes
//note: this must appear below "common.h" which contains
//the definition for CILK_IVARS
#include <cilk/common.h>
#include "concurrent_cilk_internal.h"
#include <cilk/concurrent_queue.h>

void self_steal(__cilkrts_worker *w)
{
  cilk_dbg(SCHED, "[self_steal] %d/%p\n", w->self, w);

  full_frame *child_ff, *parent_ff = w->l->frame_ff;
  __cilkrts_stack_frame *sf;
  __cilkrts_stack *sd;
  char *sp;


  if (!can_steal_from(w)) return;

  if (__cilkrts_mutex_trylock(w, &w->l->steal_lock)) {


    sd = __cilkrts_get_stack(w);
    if_f(NULL == sd) goto unlock;

    sf = __cilkrts_pop_tail(w);
    if_f(NULL == sf) goto unlock;

    sf->flags |= CILK_FRAME_SF_PEDIGREE_UNSYNCHED;
    child_ff = __cilkrts_make_full_frame(w, sf);
    child_ff->parent = parent_ff;
    child_ff->stack_self = sd;
    child_ff->sync_master = w;
    child_ff->is_call_child = 0;

    cilk_dbg(FRAME, "[self_steal] w %d/%p sf %p parent %p child %p", w->self, w, sf, parent_ff, child_ff);

    parent_ff->call_stack->flags |= CILK_FRAME_STOLEN;
    push_child(parent_ff, child_ff);
    sp = __cilkrts_stack_to_pointer(sd, sf);

    //looks like the last two arguments are never used? wtf...
    __cilkrts_bind_stack(child_ff, sp, NULL, NULL);

    sf->flags |= CILK_FRAME_SELF_STEAL;
    sf->call_parent = parent_ff->call_stack;
    make_unrunnable(w, child_ff, sf, 1, "self_steal");
    sf->flags &= ~CILK_FRAME_STOLEN;
    w->l->frame_ff = NULL; //needed for assertion in do_work

    __cilkrts_push_next_frame(w,child_ff);
unlock:
    __cilkrts_mutex_unlock(w, &w->l->steal_lock);
    return;
  } 
  cilk_dbg(1, "[self steal] could not get log on worker %d/%p\n", w->self, w);
}

void concurrent_sched(__cilkrts_worker *w, void *args)
{
  
  full_frame *ff;

  ff = pop_next_frame(w);

  // If there is no work on the queue, try to steal some.
  if (NULL == ff) {

    START_INTERVAL(w, INTERVAL_STEALING) {
      if (w->l->type != WORKER_USER && w->l->team != NULL) {
        // At this point, the worker knows for certain that it has run
        // out of work.  Therefore, it loses its team affiliation.  User
        // workers never change teams, of course.
        __cilkrts_worker_lock(w);
        w->l->team = NULL;
        __cilkrts_worker_unlock(w);
      }

      if (can_steal_from(w)){
        self_steal(w);
      } else {
        random_steal(w);
      }

    } STOP_INTERVAL(w, INTERVAL_STEALING);

    // If the steal was successful, then the worker has populated its next
    // frame with the work to resume.
    ff = pop_next_frame(w);
    if (NULL == ff) {
      // Punish the worker for failing to steal.
      // No quantum for you!
      __cilkrts_yield();
      w->l->steal_failure_count++;
      return;
    } else {
      // Reset steal_failure_count since there is obviously still work to
      // be done.
      w->l->steal_failure_count = 0;
    }
  }

  CILK_ASSERT(ff);

  // Do the work that was on the queue or was stolen.
  START_INTERVAL(w, INTERVAL_WORKING) {
    do_work(w, ff);
    ITT_SYNC_SET_NAME_AND_PREPARE(w, w->l->sync_return_address);
  } STOP_INTERVAL(w, INTERVAL_WORKING);
}

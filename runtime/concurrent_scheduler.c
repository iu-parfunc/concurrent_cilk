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

/* Pop a call stack from TAIL.  Return the call stack, or NULL if the
   queue is empty */
inline static __cilkrts_stack_frame *pop_tail(__cilkrts_worker *w)
{
  //ASSERT <<< worker lock owned!
  __cilkrts_stack_frame *sf;
  __cilkrts_stack_frame *volatile *tail = w->tail;

  if (w->head < tail) {
    --tail;
    sf = *tail;
    w->tail = tail;

  } else {
    sf = 0;
  }
  return sf;
}

inline void self_steal(__cilkrts_worker *w)
{
  full_frame *child_ff, *parent_ff = w->l->frame_ff;
  __cilkrts_stack_frame *sf;
  __cilkrts_stack *sd;
  char *sp;

  if (!can_steal_from(w)) return;

  if(! __cilkrts_mutex_trylock(w, &w->l->lock)) return;
  /* tell thieves to stay out of the way */
  w->l->do_not_steal = 1;
  __cilkrts_fence(); /* probably redundant */

  sd = __cilkrts_get_stack(w);
  if_f(NULL == sd) goto unlock;

  sf = pop_tail(w);
  if_f(NULL == sf) goto unlock;

  //do we want these unsynced?
  sf->flags |= CILK_FRAME_SF_PEDIGREE_UNSYNCHED;
  //this isn't a real unsyncing, we are stealing up our own task queue, like serial work.
  //sf->flags |= CILK_FRAME_UNSYNCHED;

  child_ff = __cilkrts_make_full_frame(w, sf);
  child_ff->parent = parent_ff;
  child_ff->stack_self = sd;
  child_ff->sync_master = w;
  child_ff->is_call_child = 0;

  cilk_dbg(SCHED|FRAME, "[self_steal] w %d/%p sf %p parent %p child %p\n",
      w->self, w, sf, parent_ff, child_ff);

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
  w->self_steal_offset++; 

unlock:
  __cilkrts_worker_unlock(w);
}

typedef enum sched_state_t { STEALING, WORKING, EVENT_WAKEUP } sched_state_t;

inline static void  reset_worker(__cilkrts_worker *w)
{
  if (w->l->type != WORKER_USER && w->l->team != NULL) {
    BEGIN_WITH_WORKER_LOCK(w) {
      w->l->team = NULL;
    } END_WITH_WORKER_LOCK(w);
  }
}

#define HAS_WORK 0x1
#define CAN_WAKEUP_IVARS 0x2
#define CAN_SELF_STEAL 0x4

inline static sched_state_t build_sched_state(__cilkrts_worker *w)
{
  uint32_t state = 0;

  full_frame *ff = pop_next_frame(w);

  state |= (ff == NULL)               * HAS_WORK;
  state |= can_steal_from(w)          * CAN_SELF_STEAL;
  state |= q_is_empty(w->ready_queue) * CAN_WAKEUP_IVARS;

  return state;
}

void concurrent_sched(__cilkrts_worker *w, void *args)
{

  full_frame *ff;
  uint32_t state = build_sched_state(w);

  reset_worker(w);

  switch (state) {
    case CAN_WAKEUP_IVARS:
      restore_ready_computation(w); 
      CILK_ASSERT(0); 
    case HAS_WORK: 
      break;
    case CAN_SELF_STEAL:
      self_steal(w); break;
    default:
      w->self_steal_offset = 0;
      w->l->frame_ff = 0;
      random_steal(w);
  }


  // If the steal was successful, then the worker has populated its next
  // frame with the work to resume.
  ff = pop_next_frame(w);
  if (NULL == ff) {
    // Punish the worker for failing to steal.
    // No quantum for you!
    __cilkrts_yield();
    w->l->steal_failure_count++;
    return;
  } 

  // Reset steal_failure_count since there is obviously still work to
  // be done.
  w->l->steal_failure_count = 0;
  do_work(w, ff);
  ITT_SYNC_SET_NAME_AND_PREPARE(w, w->l->sync_return_address);
}


#include "scheduler.h"
#include "full_frame.h"
#include "os.h"
#include "stacks.h"
#include "sysdep.h"
#include "sync.h"

#include "cilk-ittnotify.h"

// ICL: Don't complain about loss of precision in myrand
// I tried restoring the warning after the function, but it didn't
// suppress it
#ifdef _WIN32
#   pragma warning(disable: 2259)
#endif

/*************************************************************
                PSEUDO RANDOM NUMBER GENERATOR
*************************************************************/

/*
 * Pseudo-random generator defined by the congruence S' = 69070 * S
 * mod (2^32 - 5).  Marsaglia (CACM July 1993) says on page 107 that
 * this is a ``good one''.  There you go.
 *
 * The literature makes a big fuss about avoiding the division, but
 * for us it is not worth the hassle.
 */
static const unsigned RNGMOD = ((1ULL << 32) - 5);
static const unsigned RNGMUL = 69070U;

unsigned myrand(__cilkrts_worker *w)
{
  unsigned state = w->l->rand_seed;
  state = (unsigned)((RNGMUL * (unsigned long long)state) % RNGMOD);
  w->l->rand_seed = state;
  return state;
}

void mysrand(__cilkrts_worker *w, unsigned seed)
{
  seed %= RNGMOD;
  seed += (seed == 0); /* 0 does not belong to the multiplicative
                          group.  Use 1 instead */
  w->l->rand_seed = seed;
}

/*************************************************************
   THE protocol:
*************************************************************/
/*
  This is a protocol for work stealing that minimize the
  overhead on the victim.

  The protocol uses three shared pointes into the victim's deque: T
  (the ``tail''), H (the ``head'') and E (the ``exception''),
  with H <= E, H <= T.  (NB: "exception," in this case has nothing to do with
  C++ throw-catch exceptions -- it refers only to a non-normal return, i.e., a
  steal or similar scheduling exception.)

  Stack frames P, where H <= E < T, are available for stealing. 

  The victim operates on the T end of the stack.  The frame being
  worked on by the victim is not on the stack.  To push, the victim
  stores *T++=frame.  To pop, it obtains frame=*--T.

  After decrementing T, the condition E > T signals to the victim that
  it should invoke the runtime system ``THE'' exception handler.  The
  pointer E can become INFINITY, in which case the victim must invoke
  the THE exception handler as soon as possible.

  See "The implementation of the Cilk-5 multithreaded language", PLDI 1998,
  http://portal.acm.org/citation.cfm?doid=277652.277725, for more information
  on the THE protocol.
*/


void increment_E(__cilkrts_worker *victim)
{
    __cilkrts_stack_frame *volatile *tmp;

    // The currently executing worker must own the worker lock to touch
    // victim->exc
    ASSERT_WORKER_LOCK_OWNED(victim);

    tmp = victim->exc;
    if (tmp != EXC_INFINITY) {
        /* On most x86 this pair of operations would be slightly faster
           as an atomic exchange due to the implicit memory barrier in
           an atomic instruction. */
        victim->exc = tmp + 1;
        __cilkrts_fence();
    }
}

void decrement_E(__cilkrts_worker *victim)
{
    __cilkrts_stack_frame *volatile *tmp;

    // The currently executing worker must own the worker lock to touch
    // victim->exc
    ASSERT_WORKER_LOCK_OWNED(victim);

    tmp = victim->exc;
    if (tmp != EXC_INFINITY) {
        /* On most x86 this pair of operations would be slightly faster
           as an atomic exchange due to the implicit memory barrier in
           an atomic instruction. */
        victim->exc = tmp - 1;
        __cilkrts_fence(); /* memory fence not really necessary */
    }
}

#if 0
/* for now unused, will be necessary if we implement abort */
static void signal_THE_exception(__cilkrts_worker *wparent)
{
    wparent->exc = EXC_INFINITY;
    __cilkrts_fence();
}
#endif

void reset_THE_exception(__cilkrts_worker *w)
{
    // The currently executing worker must own the worker lock to touch
    // w->exc
    ASSERT_WORKER_LOCK_OWNED(w);

    w->exc = w->head;
    __cilkrts_fence();
}

/* Return true if the frame can be stolen, false otherwise */
int dekker_protocol(__cilkrts_worker *victim)
{
    // increment_E and decrement_E are going to touch victim->exc.  The
    // currently executing worker must own victim's lock before they can
    // modify it
    ASSERT_WORKER_LOCK_OWNED(victim);

    /* ASSERT(E >= H); */

    increment_E(victim);

    /* ASSERT(E >= H + 1); */
    if (can_steal_from(victim)) {
        /* success, we can steal victim->head and set H <- H + 1
           in detach() */
        return 1;
    } else {
        /* failure, restore previous state */
        decrement_E(victim);
        return 0;    
    }
}

/*************************************************************
             RANDOM SCHEDULER IMPLEMENTATION
*************************************************************/

void random_steal(__cilkrts_worker *w)
{
  __cilkrts_worker *victim;
  __cilkrts_stack *sd;
  int n;
  int success = 0;

  // Nothing's been stolen yet. When true, this will flag
  // setup_for_execution_pedigree to increment the pedigree
  w->l->work_stolen = 0;

  /* If the user has disabled stealing (using the debugger) we fail */
  if (__builtin_expect(w->g->stealing_disabled, 0))
    return;

  CILK_ASSERT(w->l->type == WORKER_SYSTEM || w->l->team == w);

  /* If there is only one processor work can still be stolen.
     There must be only one worker to prevent stealing. */
  CILK_ASSERT(w->g->total_workers > 1);

  /* Verify that we can get a stack.  If not, no need to continue. */
  sd = __cilkrts_get_stack(w);
  if (NULL == sd) {
    return;
  }

  /* pick random *other* victim */
  n = myrand(w) % (w->g->total_workers - 1); if (n >= w->self) ++n;
  victim = w->g->workers[n];

  /* do not steal from self */
  CILK_ASSERT (victim != w);

  /* Execute a quick check before engaging in the THE protocol.
     Avoid grabbing locks if there is nothing to steal. */
  if (!can_steal_from(victim)) {
    NOTE_INTERVAL(w, INTERVAL_STEAL_FAIL_EMPTYQ);
    __cilkrts_release_stack(w, sd);
    return;
  }

  /* Attempt to steal work from the victim */
  if (worker_trylock_other(w, victim)) {
    if (w->l->type == WORKER_USER && victim->l->team != w) {

      // Fail to steal if this is a user worker and the victim is not
      // on this team.  If a user worker were allowed to steal work
      // descended from another user worker, the former might not be
      // done with its work by the time it was needed to resume and
      // unbind.  Therefore, user workers are not permitted to change
      // teams.

      // There is no race on the victim's team because the victim cannot
      // change its team until it runs out of work to do, at which point
      // it will try to take out its own lock, and this worker already
      // holds it.
      NOTE_INTERVAL(w, INTERVAL_STEAL_FAIL_USER_WORKER);

    } else if (victim->l->frame_ff) {
      // A successful steal will change victim->frame_ff, even
      // though the victim may be executing.  Thus, the lock on
      // the victim's deque is also protecting victim->frame_ff.
      if (dekker_protocol(victim)) {
        START_INTERVAL(w, INTERVAL_STEAL_SUCCESS) {
          success = 1;
          detach_for_steal(w, victim, sd);
#if REDPAR_DEBUG >= 1
          fprintf(stderr, "Wkr %d stole from victim %d, sd = %p\n",
              w->self, victim->self, sd);
#endif

          // The use of victim->self contradicts our
          // classification of the "self" field as 
          // local.  But since this code is only for
          // debugging, it is ok.
          DBGPRINTF ("%d-%p: Stealing work from worker %d\n"
              "            sf: %p, call parent: %p\n",
              w->self, GetCurrentFiber(), victim->self,
              w->l->next_frame_ff->call_stack,
              w->l->next_frame_ff->call_stack->call_parent);
        } STOP_INTERVAL(w, INTERVAL_STEAL_SUCCESS);
      } else {
        NOTE_INTERVAL(w, INTERVAL_STEAL_FAIL_DEKKER);
      }
    } else {
      NOTE_INTERVAL(w, INTERVAL_STEAL_FAIL_EMPTYQ);
    }
    worker_unlock_other(w, victim);
  } else {
    NOTE_INTERVAL(w, INTERVAL_STEAL_FAIL_LOCK);
  }

  // Record whether work was stolen.  When true, this will flag
  // setup_for_execution_pedigree to increment the pedigree
  w->l->work_stolen = success;

  if (0 == success) {
    // failed to steal work.  Return the stack to the pool.
    __cilkrts_release_stack(w, sd);
  }
}

/* w should be the currently executing worker.  
 * loot_sf is the youngest stack frame in the call stack being 
 *   unrolled (i.e., the most deeply nested stack frame.)
 *
 * When this method is called for a steal, loot_sf should be on a
 * victim worker which is different from w.
 * For CILK_FORCE_REDUCE, the victim worker will equal w.
 *
 * Before execution, the __cilkrts_stack_frame's have pointers from
 * older to younger, i.e., a __cilkrts_stack_frame points to parent.
 *
 * This method creates a full frame for each __cilkrts_stack_frame in
 * the call stack, with each full frame also pointing to its parent. 
 *
 * The method returns the full frame created for loot_sf, i.e., the
 * youngest full frame.
 */
full_frame *unroll_call_stack(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *const loot_sf)
{
  __cilkrts_stack_frame *sf = loot_sf;
  __cilkrts_stack_frame *rev_sf = 0;
  __cilkrts_stack_frame *t_sf;

  CILK_ASSERT(sf);
  /*CILK_ASSERT(sf->call_parent != sf);*/

  /* The leafmost frame is unsynched. */
  if (sf->worker != w)
    sf->flags |= CILK_FRAME_UNSYNCHED;

  /* Reverse the call stack to make a linked list ordered from parent
     to child.  sf->call_parent points to the child of SF instead of
     the parent.  */
  do {
    t_sf = (sf->flags & (CILK_FRAME_DETACHED|CILK_FRAME_STOLEN|CILK_FRAME_LAST))? 0 : sf->call_parent;
    sf->call_parent = rev_sf;
    rev_sf = sf;
    sf = t_sf;
  } while (sf);
  sf = rev_sf;

  /* Promote each stack frame to a full frame in order from parent
     to child, following the reversed list we just built. */
  make_unrunnable(w, ff, sf, sf == loot_sf, "steal 1");
  /* T is the *child* of SF, because we have reversed the list */
  for (t_sf = __cilkrts_advance_frame(sf); t_sf;
      sf = t_sf, t_sf = __cilkrts_advance_frame(sf)) {
    ff = make_child(w, ff, t_sf, NULL);
    make_unrunnable(w, ff, t_sf, t_sf == loot_sf, "steal 2");
  }

  /* XXX What if the leafmost frame does not contain a sync
     and this steal is from promote own deque? */
  /*sf->flags |= CILK_FRAME_UNSYNCHED;*/

  CILK_ASSERT(!sf->call_parent);
  return ff;
}

/* detach the top of the deque frame from the VICTIM and install a new
   CHILD frame in its place */
void detach_for_steal(__cilkrts_worker *w,
    __cilkrts_worker *victim,
    __cilkrts_stack *sd)
{
  /* ASSERT: we own victim->lock */

  full_frame *parent_ff, *child_ff, *loot_ff;
  __cilkrts_stack_frame *volatile *h;
  __cilkrts_stack_frame *sf;

  w->l->team = victim->l->team;

  CILK_ASSERT(w->l->frame_ff == 0 || w == victim);

  h = victim->head;

  CILK_ASSERT(*h);

  victim->head = h + 1;

  parent_ff = victim->l->frame_ff;
  BEGIN_WITH_FRAME_LOCK(w, parent_ff) {
    /* parent no longer referenced by victim */
    decjoin(parent_ff);

    /* obtain the victim call stack */
    sf = *h;

    /* perform system-dependent normalizations */
    /*__cilkrts_normalize_call_stack_on_steal(sf);*/

    /* unroll PARENT_FF with call stack SF, adopt the youngest
       frame LOOT.  If loot_ff == parent_ff, then we hold loot_ff->lock,
       otherwise, loot_ff is newly created and we can modify it without
       holding its lock. */
    loot_ff = unroll_call_stack(w, parent_ff, sf);

    fprintf(stderr, "[W=%d, victim=%d, desc=detach, parent_ff=%p, loot=%p]\n",
        w->self, victim->self,
        parent_ff, loot_ff);
#if REDPAR_DEBUG >= 3
    fprintf(stderr, "[W=%d, victim=%d, desc=detach, parent_ff=%p, loot=%p]\n",
        w->self, victim->self,
        parent_ff, loot_ff);
#endif

    if (WORKER_USER == victim->l->type &&
        NULL == victim->l->last_full_frame) {
      // Mark this looted frame as special: only the original user worker
      // may cross the sync.
      // 
      // This call is a shared access to
      // victim->l->last_full_frame.
      set_sync_master(victim, loot_ff);
    }

    /* LOOT is the next frame that the thief W is supposed to
       run, unless the thief is stealing from itself, in which
       case the thief W == VICTIM executes CHILD and nobody
       executes LOOT. */
    if (w == victim) {
      /* Pretend that frame has been stolen */
      loot_ff->call_stack->flags |= CILK_FRAME_UNSYNCHED;
      loot_ff->simulated_stolen = 1;
    }
    else
      __cilkrts_push_next_frame(w, loot_ff);

    // After this "push_next_frame" call, w now owns loot_ff.
    child_ff = make_child(w, loot_ff, 0, sd);

    BEGIN_WITH_FRAME_LOCK(w, child_ff) {
      /* install child in the victim's work queue, taking
         the parent_ff's place */
      /* child is referenced by victim */
      incjoin(child_ff);

      // With this call, w is bestowing ownership of the newly
      // created frame child_ff to the victim, and victim is
      // giving up ownership of parent_ff.
      //
      // Worker w will either take ownership of parent_ff
      // if parent_ff == loot_ff, or parent_ff will be
      // suspended.
      //
      // Note that this call changes the victim->frame_ff
      // while the victim may be executing.
      make_runnable(victim, child_ff);
    } END_WITH_FRAME_LOCK(w, child_ff);
  } END_WITH_FRAME_LOCK(w, parent_ff);
}

/*
 * Try to do work.  If there is none available, try to steal some and do it.
 */
void random_work_steal_sched(__cilkrts_worker *w, void *args)
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

      random_steal(w);

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

//--------------------------------------------------------

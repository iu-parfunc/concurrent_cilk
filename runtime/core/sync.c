#include "full_frame.h"
#include "scheduler.h"
#include "sysdep.h"
#include "stacks.h"
#include "sync.h"

/*
 * Identify the single worker that is allowed to cross a sync in this frame.  A
 * thief should call this function when it is the first to steal work from a
 * user worker.  "First to steal work" may mean that there has been parallelism
 * in the user worker before, but the whole team sync'd, and this is the first
 * steal after that.
 *
 * This should happen while holding the worker and frame lock.
 */
void set_sync_master(__cilkrts_worker *w, full_frame *ff)
{
    w->l->last_full_frame = ff;
    ff->sync_master = w;
}

/*
 * The sync that ends all parallelism for a particular user worker is about to
 * be crossed.  Decouple the worker and frame.
 *
 * No locks need to be held since the user worker isn't doing anything, and none
 * of the system workers can steal from it.  But unset_sync_master() should be
 * called before the user worker knows about this work (i.e., before it is
 * inserted into the w->l->next_frame_ff is set).
 */
void unset_sync_master(__cilkrts_worker *w, full_frame *ff)
{
    CILK_ASSERT(WORKER_USER == w->l->type);
    CILK_ASSERT(ff->sync_master == w);
    ff->sync_master = NULL;
    w->l->last_full_frame = NULL;
}

void __cilkrts_mark_synched(full_frame *ff)
{
  ff->call_stack->flags &= ~CILK_FRAME_UNSYNCHED;
  ff->simulated_stolen = 0;
}

NORETURN __cilkrts_c_sync(__cilkrts_worker *w, __cilkrts_stack_frame *sf_at_sync)
{
  full_frame *ff; 

  // Claim: This read of w->l->frame_ff can occur without
  // holding the worker lock because when w has reached a sync
  // and entered the runtime (because it stalls), w's deque is empty
  // and no one else can steal and change w->l->frame_ff.

  ff = w->l->frame_ff;
#ifdef _WIN32
  __cilkrts_save_exception_state(w, ff);
#else
  // Move any pending exceptions into the full frame
  //CILK_ASSERT(NULL == ff->pending_exception);
  //ff->pending_exception = w->l->pending_exception;
  //w->l->pending_exception = NULL;
#endif

  //w = execute_reductions_for_sync(w, ff, sf_at_sync);

  longjmp_into_runtime(w, do_sync, sf_at_sync);
}

void do_sync(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf)
{
  int abandoned = 1;     
  START_INTERVAL(w, INTERVAL_SYNC_CHECK) {
    BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) {
      ff = w->l->frame_ff;
      w->l->frame_ff = NULL;
      // Conceptually, after clearing w->l->frame_ff, 
      // w no longer owns the full frame ff.
      // The next time another (possibly different) worker takes
      // ownership of ff will be at a provably_good_steal on ff. 

      CILK_ASSERT(ff);
      BEGIN_WITH_FRAME_LOCK(w, ff) {
        CILK_ASSERT(sf->call_parent == 0);
        CILK_ASSERT(sf->flags & CILK_FRAME_UNSYNCHED);

        /* A frame entering a nontrivial sync always has a
           stack_self.  A topmost frame after a sync does
           not; it is back on the caller's stack. */
        CILK_ASSERT(ff->stack_self || ff->simulated_stolen);

        // Notify TBB that we're orphaning the stack. We'll reclaim it
        // again if we continue
        __cilkrts_invoke_stack_op(w, CILK_TBB_STACK_ORPHAN, ff->stack_self);

        /* if (ff->stack_self) see above comment */ {
          __cilkrts_stack *s = ff->stack_self;
          ff->stack_self = NULL;
          __cilkrts_release_stack(w, s);
        }

        // Update the frame's pedigree information if this is an ABI 1 or later
        // frame
        if (CILK_FRAME_VERSION_VALUE(sf->flags) >= 1)
        {
          sf->parent_pedigree.rank = w->pedigree.rank;
          sf->parent_pedigree.parent = w->pedigree.parent;

          // Note that the pedigree rank needs to be updated
          // when setup_for_execution_pedigree runs
          sf->flags |= CILK_FRAME_SF_PEDIGREE_UNSYNCHED;
        }

        /* the decjoin() occurs in provably_good_steal() */
        abandoned = provably_good_steal(w, ff);

      } END_WITH_FRAME_LOCK(w, ff);
    } END_WITH_WORKER_LOCK_OPTIONAL(w);
  } STOP_INTERVAL(w, INTERVAL_SYNC_CHECK);

#ifdef ENABLE_NOTIFY_ZC_INTRINSIC
  // If we can't make any further progress on this thread, tell Inspector
  // that we're abandoning the work and will go find something else to do.
  if (abandoned)
  {
    __notify_zc_intrinsic("cilk_sync_abandon", 0);
  }
#endif // defined ENABLE_NOTIFY_ZC_INTRINSIC

  return; /* back to scheduler loop */
}

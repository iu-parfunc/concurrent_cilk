/* scheduler.c                  -*-C-*-
 *
 *************************************************************************
 *
 * Copyright (C) 2007-2012 
 * Intel Corporation
 * 
 * This file is part of the Intel Cilk Plus Library.  This library is free
 * software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 * 
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 **************************************************************************/

/*
 * Cilk scheduler
 */

#include "scheduler.h"
#include "bug.h"
#include "os.h"
#include "os_mutex.h"
#include "local_state.h"
#include "signal_node.h"
#include "full_frame.h"
#include "stacks.h"
#include "sysdep.h"
#include "except.h"
#include "cilk_malloc.h"
#include "pedigrees.h"

#include <string.h> /* memcpy */
#include <stdio.h>  // sprintf
#include <stdlib.h> // malloc, free, abort

#ifdef _WIN32
#   pragma warning(disable:1786)   // disable warning: sprintf is deprecated
#   include "sysdep-win.h"
#endif  // _WIN32

//concurrent cilk conditional includes
//note: this must appear below "common.h" which contains
//the definition for CILK_IVARS
#ifdef CILK_IVARS
#include <cilk/common.h>
#include "concurrent_cilk_internal.h"
#include "concurrent_queue.h"
#endif

// ICL: Don't complain about conversion from pointer to same-sized integral
// type in __cilkrts_put_stack.  That's why we're using ptrdiff_t
#ifdef _WIN32
#   pragma warning(disable: 1684)
#endif

#include "cilk/cilk_api.h"
#include "frame_malloc.h"
#include "metacall_impl.h"
#include "reducer_impl.h"
#include "cilk-tbb-interop.h"
#include "cilk-ittnotify.h"
#include "stats.h"
// ICL: Don't complain about loss of precision in myrand
// I tried restoring the warning after the function, but it didn't
// suppress it
#ifdef _WIN32
#   pragma warning(disable: 2259)
#endif

#ifndef _WIN32
#   include <unistd.h>
#endif

//concurrent cilk conditional includes
//note: this must appear below "common.h" which contains
//the definition for CILK_IVARS
#ifdef CILK_IVARS
#include "concurrent_cilk_internal.h"
#include <concurrent_queue.h>
#endif

//#define DEBUG_LOCKS 1
#ifdef DEBUG_LOCKS
// The currently executing worker must own this worker's lock
#   define ASSERT_WORKER_LOCK_OWNED(w) \
        { \
            __cilkrts_worker *tls_worker = __cilkrts_get_tls_worker(); \
            CILK_ASSERT((w)->l->lock.owner == tls_worker); \
        }
#else
#   define ASSERT_WORKER_LOCK_OWNED(w)
#endif // DEBUG_LOCKS

// Options for the scheduler.
enum schedule_t { SCHEDULE_RUN,
                  SCHEDULE_WAIT,
                  SCHEDULE_EXIT };


// Verify that "w" is the worker we are currently executing on.
// Because this check is expensive, this method is usually a no-op.
static inline void verify_current_wkr(__cilkrts_worker *w)
{
#if REDPAR_DEBUG >= 3
    // Lookup the worker from TLS and compare to w. 
    __cilkrts_worker* tmp = __cilkrts_get_tls_worker();
    if (w != tmp) {
        fprintf(stderr, "Error.  W=%d, actual worker =%d...\n",
                w->self,
                tmp->self);
    }
    CILK_ASSERT(w == tmp);
#endif
}                                                            

static enum schedule_t worker_runnable(__cilkrts_worker *w);

// Scheduling-stack functions:
 void do_return_from_spawn (__cilkrts_worker *w,
                                  full_frame *ff,
                                  __cilkrts_stack_frame *sf);
static void do_sync (__cilkrts_worker *w,
                     full_frame *ff,
                     __cilkrts_stack_frame *sf);

#ifndef _WIN32
    // TBD: definition of max() for Linux.
#   define max(a, b) ((a) < (b) ? (b) : (a))
#endif

void __cilkrts_dump_stats_to_stderr(global_state_t *g)
{
#ifdef CILK_PROFILE
    int i;
    for (i = 0; i < g->total_workers; ++i)
        __cilkrts_accum_stats(&g->stats, &g->workers[i]->l->stats);
    dump_stats_to_file(stderr, &g->stats);
#endif
    fprintf(stderr,
            "CILK PLUS Thread Info: P=%d, Q=%d\n",
            g->P,
            g->Q);
    fprintf(stderr,
            "CILK PLUS RUNTIME MEMORY USAGE: %lld bytes",
            (long long)g->frame_malloc.allocated_from_os);
#ifdef CILK_PROFILE
    if (g->stats.stack_hwm)
        fprintf(stderr, ", %ld stacks", g->stats.stack_hwm);
#endif
    fputc('\n', stderr);
}

static void validate_worker(__cilkrts_worker *w)
{
    /* check the magic numbers, for debugging purposes */
    if (w->l->worker_magic_0 != WORKER_MAGIC_0 ||
        w->l->worker_magic_1 != WORKER_MAGIC_1)
        abort_because_rts_is_corrupted();
}

static void double_link(full_frame *left_ff, full_frame *right_ff)
{
    if (left_ff)
        left_ff->right_sibling = right_ff;
    if (right_ff)
        right_ff->left_sibling = left_ff;
}

/* add CHILD to the right of all children of PARENT */
static void push_child(full_frame *parent_ff, full_frame *child_ff)
{
    double_link(parent_ff->rightmost_child, child_ff);
    double_link(child_ff, 0);
    parent_ff->rightmost_child = child_ff;
}

/* unlink CHILD from the list of all children of PARENT */
static void unlink_child(full_frame *parent_ff, full_frame *child_ff)
{
    double_link(child_ff->left_sibling, child_ff->right_sibling);

    if (!child_ff->right_sibling) {
        /* this is the rightmost child -- update parent link */
        CILK_ASSERT(parent_ff->rightmost_child == child_ff);
        parent_ff->rightmost_child = child_ff->left_sibling;
    }
    child_ff->left_sibling = child_ff->right_sibling = 0; /* paranoia */
}

static void incjoin(full_frame *ff)
{
    ++ff->join_counter;
}

static int decjoin(full_frame *ff)
{
    CILK_ASSERT(ff->join_counter > 0);
    return (--ff->join_counter);
}

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

static unsigned myrand(__cilkrts_worker *w)
{
    unsigned state = w->l->rand_seed;
    state = (unsigned)((RNGMUL * (unsigned long long)state) % RNGMOD);
    w->l->rand_seed = state;
    return state;
}

static void mysrand(__cilkrts_worker *w, unsigned seed)
{
    seed %= RNGMOD;
    seed += (seed == 0); /* 0 does not belong to the multiplicative
                            group.  Use 1 instead */
    w->l->rand_seed = seed;
}

/* W grabs its own lock */
void __cilkrts_worker_lock(__cilkrts_worker *w)
{
    validate_worker(w);
    CILK_ASSERT(w->l->do_not_steal == 0);

    /* tell thieves to stay out of the way */
    w->l->do_not_steal = 1;
    __cilkrts_fence(); /* probably redundant */

    __cilkrts_mutex_lock(w, &w->l->lock);
}

void __cilkrts_worker_unlock(__cilkrts_worker *w)
{
    __cilkrts_mutex_unlock(w, &w->l->lock);
    CILK_ASSERT(w->l->do_not_steal == 1);
    /* The fence is probably redundant.  Use a release
       operation when supported (gcc and compatibile);
       that is faster on x86 which serializes normal stores. */
#if defined __GNUC__ && (__GNUC__ * 10 + __GNUC_MINOR__ > 43 || __ICC >= 1110)
    __sync_lock_release(&w->l->do_not_steal);
#else
    w->l->do_not_steal = 0;
    __cilkrts_fence(); /* store-store barrier, redundant on x86 */
#endif
}

/* try to acquire the lock of some *other* worker */
static int worker_trylock_other(__cilkrts_worker *w,
                                __cilkrts_worker *other)
{
    int status = 0;

    validate_worker(other);

    /* This protocol guarantees that, after setting the DO_NOT_STEAL
       flag, worker W can enter its critical section after waiting for
       the thief currently in the critical section (if any) and at
       most one other thief.  

       This requirement is overly paranoid, but it should protect us
       against future nonsense from OS implementors.
    */

    /* compete for the right to disturb OTHER */
    if (__cilkrts_mutex_trylock(w, &other->l->steal_lock)) {
        if (other->l->do_not_steal) {
            /* leave it alone */
        } else {
            status = __cilkrts_mutex_trylock(w, &other->l->lock);
        }
        __cilkrts_mutex_unlock(w, &other->l->steal_lock);
    }


    return status;
}

static void worker_unlock_other(__cilkrts_worker *w,
                                __cilkrts_worker *other)
{
    __cilkrts_mutex_unlock(w, &other->l->lock);
}


/* Lock macro Usage:
    BEGIN_WITH_WORKER_LOCK(w) {
        statement;
        statement;
        BEGIN_WITH_FRAME_LOCK(w, ff) {
            statement;
            statement;
        } END_WITH_FRAME_LOCK(w, ff);
    } END_WITH_WORKER_LOCK(w);
 */
#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)

// TBD(jsukha): These are worker lock acquistions on
// a worker whose deque is empty.  My conjecture is that we
// do not need to hold the worker lock at these points.
// I have left them in for now, however.
//
// #define REMOVE_POSSIBLY_OPTIONAL_LOCKS
#ifdef REMOVE_POSSIBLY_OPTIONAL_LOCKS
    #define BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) do
    #define END_WITH_WORKER_LOCK_OPTIONAL(w)   while (0)
#else
    #define BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) __cilkrts_worker_lock(w); do
    #define END_WITH_WORKER_LOCK_OPTIONAL(w)   while (__cilkrts_worker_unlock(w), 0)
#endif


#define BEGIN_WITH_FRAME_LOCK(w, ff)                                     \
    do { full_frame *_locked_ff = ff; __cilkrts_frame_lock(w, _locked_ff); do

#define END_WITH_FRAME_LOCK(w, ff)                       \
    while (__cilkrts_frame_unlock(w, _locked_ff), 0); } while (0)

/* W becomes the owner of F and F can be stolen from W */
static void make_runnable(__cilkrts_worker *w, full_frame *ff)
{
    w->l->frame_ff = ff;

    /* CALL_STACK is invalid (the information is stored implicitly in W) */
    ff->call_stack = 0;
}

/*
 * The worker parameter is unused, except for print-debugging purposes.
 */
static void make_unrunnable(__cilkrts_worker *w,
                            full_frame *ff,
                            __cilkrts_stack_frame *sf,
                            int state_valid,
                            const char *why)
{
    /* CALL_STACK becomes valid again */
    ff->call_stack = sf;

    if (sf) {
#if CILK_LIB_DEBUG
        if (__builtin_expect(sf->flags & CILK_FRAME_EXITING, 0))
            __cilkrts_bug("W%d suspending exiting frame %p/%p\n", w->self, ff, sf);
#endif
        sf->flags |= CILK_FRAME_STOLEN | CILK_FRAME_SUSPENDED;
        sf->worker = 0;

        if (state_valid)
            __cilkrts_put_stack(ff, sf);

        /* perform any system-dependent action, such as saving the
           state of the stack */
        __cilkrts_make_unrunnable_sysdep(w, ff, sf, state_valid, why);
    }
}


/* Push the next full frame to be made active in this worker and increment its
 * join counter.  __cilkrts_push_next_frame and pop_next_frame work on a
 * one-element queue.  This queue is used to communicate across the runtime
 * from the code that wants to activate a frame to the code that can actually
 * begin execution on that frame.  They are asymetrical in that push
 * increments the join counter but pop does not decrement it.  Rather, a
 * single push/pop combination makes a frame active and increments its join
 * counter once. */
void __cilkrts_push_next_frame(__cilkrts_worker *w, full_frame *ff)
{
    CILK_ASSERT(ff);
    CILK_ASSERT(!w->l->next_frame_ff);
    incjoin(ff);
    w->l->next_frame_ff = ff;
}

/* Get the next full-frame to be made active in this worker.  The join count
 * of the full frame will have been incremented by the corresponding push
 * event.  See __cilkrts_push_next_frame, above.
 */
static full_frame *pop_next_frame(__cilkrts_worker *w)
{
    full_frame *ff;
    ff = w->l->next_frame_ff;
//    printf("%d popping full frame. new frame %p\n",w->self, ff);
    // Remove the frame from the next_frame field.
    //
    // If this is a user worker, then there is a chance that another worker
    // from our team could push work into our next_frame (if it is the last
    // worker doing work for this team).  The other worker's setting of the
    // next_frame could race with our setting of next_frame to NULL.  This is
    // the only possible race condition on next_frame.  However, if next_frame
    // has a non-NULL value, then it means the team still has work to do, and
    // there is no chance of another team member populating next_frame.  Thus,
    // it is safe to set next_frame to NULL, if it was populated.  There is no
    // need for an atomic op.
    if (NULL != ff) {
        w->l->next_frame_ff = NULL;
    }
    return ff;
}

/*
 * Identify the single worker that is allowed to cross a sync in this frame.  A
 * thief should call this function when it is the first to steal work from a
 * user worker.  "First to steal work" may mean that there has been parallelism
 * in the user worker before, but the whole team sync'd, and this is the first
 * steal after that.
 *
 * This should happen while holding the worker and frame lock.
 */
static void set_sync_master(__cilkrts_worker *w, full_frame *ff)
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
static void unset_sync_master(__cilkrts_worker *w, full_frame *ff)
{
    CILK_ASSERT(WORKER_USER == w->l->type);
    CILK_ASSERT(ff->sync_master == w);
    ff->sync_master = NULL;
    w->l->last_full_frame = NULL;
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

/* the infinity value of E */
#define EXC_INFINITY  ((__cilkrts_stack_frame **) (-1))

static void increment_E(__cilkrts_worker *victim)
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

static void decrement_E(__cilkrts_worker *victim)
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

static void reset_THE_exception(__cilkrts_worker *w)
{
    // The currently executing worker must own the worker lock to touch
    // w->exc
    ASSERT_WORKER_LOCK_OWNED(w);

    w->exc = w->head;
    __cilkrts_fence();
}

/* conditions under which victim->head can be stolen: */
//static
int can_steal_from(__cilkrts_worker *victim)
{
    return ((victim->head < victim->tail) && 
            (victim->head < victim->protected_tail));
}

/* Return TRUE if the frame can be stolen, false otherwise */
static int dekker_protocol(__cilkrts_worker *victim)
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

/* Link PARENT and CHILD in the spawn tree */
static full_frame *make_child(__cilkrts_worker *w, 
                              full_frame *parent_ff,
                              __cilkrts_stack_frame *child_sf,
                              __cilkrts_stack *sd)
{
    full_frame *child_ff = __cilkrts_make_full_frame(w, child_sf);

    child_ff->parent = parent_ff;
    push_child(parent_ff, child_ff);

    //DBGPRINTF("%d-          make_child - child_frame: %p, parent_frame: %p, child_sf: %p\n"
    //    "            parent - parent: %p, left_sibling: %p, right_sibling: %p, rightmost_child: %p\n"
    //    "            child  - parent: %p, left_sibling: %p, right_sibling: %p, rightmost_child: %p\n",
    //          w->self, child, parent, child_sf,
    //          parent->parent, parent->left_sibling, parent->right_sibling, parent->rightmost_child,
    //          child->parent, child->left_sibling, child->right_sibling, child->rightmost_child);

    CILK_ASSERT(parent_ff->call_stack);
    child_ff->is_call_child = (sd == NULL);

    /* PLACEHOLDER_STACK is used as non-null marker indicating that
       child should be treated as a spawn child even though we have not
       yet assigned a real stack to its parent. */
    if (sd == PLACEHOLDER_STACK)
        sd = NULL; /* Parent actually gets a null stack, for now */

    /* perform any system-dependent actions, such as capturing
       parameter passing information */
    /*__cilkrts_make_child_sysdep(child, parent);*/

    /* Child gets reducer map and stack of parent.
       Parent gets a new map and new stack. */
    child_ff->stack_self = parent_ff->stack_self;
    child_ff->sync_master = NULL;

    if (child_ff->is_call_child) {
        /* Cause segfault on any attempted access.  The parent gets
           the child map and stack when the child completes. */
        parent_ff->stack_self = 0;
    } else {
        parent_ff->stack_self = sd;
        __cilkrts_bind_stack(parent_ff,
                             __cilkrts_stack_to_pointer(parent_ff->stack_self, child_sf),
                             child_ff->stack_self,
                             child_ff->sync_master);
    }

    incjoin(parent_ff);
    return child_ff;
}

static inline __cilkrts_stack_frame *__cilkrts_advance_frame(__cilkrts_stack_frame *sf)
{
    __cilkrts_stack_frame *p = sf->call_parent;
    sf->call_parent = 0;
    return p;
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
static full_frame *unroll_call_stack(__cilkrts_worker *w, 
                                     full_frame *ff, 
                                     __cilkrts_stack_frame *const loot_sf)
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
static void detach_for_steal(__cilkrts_worker *w,
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

static void random_steal(__cilkrts_worker *w)
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

/**
 * At a provably good steal, we need to transfer the child reducer map
 * from ff->children_reducer_map into v->reducer_map, where v is the
 * worker that resumes execution of ff.
 *
 * Normally, we have v == w, where w is the currently executing
 * worker.  In the case where we are resuming a team leader on a user
 * worker, however, v might differ from w.

 * Thus, this, operation is a no-op, since we can't really move
 * ff->children_reducer_map into w here.
 *
 * Instead, this work is done in setup_for_execution_reducers().
 */
static inline void provably_good_steal_reducers(__cilkrts_worker *w,
    full_frame       *ff)
{
  // No-op.
}

/* at a provably good steal, incorporate the accumulated exceptions of
   children into the parent's exception */
static void provably_good_steal_exceptions(__cilkrts_worker *w, 
    full_frame       *ff)
{
  // ASSERT: we own ff->lock
  ff->pending_exception =
    __cilkrts_merge_pending_exceptions(w,
        ff->child_pending_exception,
        ff->pending_exception);
  ff->child_pending_exception = NULL;
}

/* At sync discard the frame's old stack and take the leftmost child's. */
static void provably_good_steal_stacks(__cilkrts_worker *w, full_frame *ff)
{
  __cilkrts_stack *s;
  s = ff->stack_self;
  ff->stack_self = ff->stack_child;
  ff->stack_child = NULL;
  if (s) {
    __cilkrts_release_stack(w, s);
  }

  /* We don't have a stack to bind right now, so use the
     BIND_PROVABLY_GOOD_STACK magic number, instead */
  __cilkrts_bind_stack(ff, ff->sync_sp, BIND_PROVABLY_GOOD_STACK, NULL);
}

static void __cilkrts_mark_synched(full_frame *ff)
{
  ff->call_stack->flags &= ~CILK_FRAME_UNSYNCHED;
  ff->simulated_stolen = 0;
}

static int provably_good_steal(__cilkrts_worker *w, full_frame *ff)
{
  // ASSERT: we hold w->lock and ff->lock

  int abandoned = 1;  // True if we can't make any more progress on this
  // thread and are going to attempt to steal work from
  // someone else

  START_INTERVAL(w, INTERVAL_PROVABLY_GOOD_STEAL) {
    if (decjoin(ff) == 0) {
      provably_good_steal_reducers(w, ff);
      provably_good_steal_exceptions(w, ff);
      provably_good_steal_stacks(w, ff);
      __cilkrts_mark_synched(ff);

      // If the original owner wants this frame back (to resume
      // it on its original thread) pass it back now.
      if (NULL != ff->sync_master) {
        // The frame wants to go back and be executed by the original
        // user thread.  We can throw caution to the wind and push the
        // frame straight onto its queue because the only way we have
        // gotten to this point of being able to continue execution of
        // the frame is if the original user worker is spinning without
        // work.

        unset_sync_master(w->l->team, ff);
        __cilkrts_push_next_frame(w->l->team, ff);

        // If this is the team leader we're not abandoning the work
        if (w == w->l->team)
          abandoned = 0;
      } else {
        __cilkrts_push_next_frame(w, ff);
        abandoned = 0;  // Continue working on this thread
      }


      // The __cilkrts_push_next_frame() call changes ownership
      // of ff to the specified worker.
    }
  } STOP_INTERVAL(w, INTERVAL_PROVABLY_GOOD_STEAL);

  return abandoned;
}

static void unconditional_steal(__cilkrts_worker *w,
    full_frame *ff)
{
  // ASSERT: we hold ff->lock

  START_INTERVAL(w, INTERVAL_UNCONDITIONAL_STEAL) {
    decjoin(ff);
    __cilkrts_push_next_frame(w, ff);
  } STOP_INTERVAL(w, INTERVAL_UNCONDITIONAL_STEAL);
}


/* CHILD is about to die.  Give its exceptions to a sibling or to the
   parent.  */
static inline void splice_exceptions_for_call(__cilkrts_worker *w,
    full_frame *parent_ff,
    full_frame *child_ff)
{
  // ASSERT: We own parent_ff->lock
  CILK_ASSERT(child_ff->is_call_child);
  CILK_ASSERT(NULL == child_ff->right_pending_exception);
  CILK_ASSERT(NULL == parent_ff->pending_exception);

  parent_ff->pending_exception = child_ff->pending_exception;
  child_ff->pending_exception = NULL;
}

/**
 * Merge exceptions for a dying child. 
 *
 * @param w                   The currently executing worker.
 * @param ff                  The child frame that is dying.
 * @param left_exception_ptr  Pointer to the exception that is to our left.
 */ 
  static inline
void splice_exceptions_for_spawn(__cilkrts_worker *w,
    full_frame *ff,
    struct pending_exception_info **left_exception_ptr)
{
  // ASSERT: parent_ff == child_ff->parent.
  // ASSERT: We own parent_ff->lock

  // Merge current exception into the slot where the left
  // exception should go.
  *left_exception_ptr =
    __cilkrts_merge_pending_exceptions(w,
        *left_exception_ptr,
        ff->pending_exception);
  ff->pending_exception = NULL;


  // Merge right exception into the slot where the left exception
  // should go.
  *left_exception_ptr =
    __cilkrts_merge_pending_exceptions(w,
        *left_exception_ptr,
        ff->right_pending_exception);
  ff->right_pending_exception = NULL;
}


static inline void splice_stacks_for_call(__cilkrts_worker *w,
    full_frame *parent_ff,
    full_frame *child_ff)
{
#if CILK_LIB_DEBUG
  if (parent_ff->call_stack)
    CILK_ASSERT(!(parent_ff->call_stack->flags & CILK_FRAME_MBZ));
#endif

  /* A synched frame does not have accumulated child reducers. */
  CILK_ASSERT(!child_ff->stack_child);
  CILK_ASSERT(child_ff->is_call_child);

  /* An attached parent has no self stack.  It may have
     accumulated child stacks or child owners, which should be
     ignored until sync. */
#ifndef CILK_IVARS
  CILK_ASSERT(!parent_ff->stack_self);
#endif
  parent_ff->stack_self = child_ff->stack_self;
  child_ff->stack_self = NULL;
}

static void finalize_child_for_call(__cilkrts_worker *w,
    full_frame *parent_ff,
    full_frame *child_ff)
{
  // ASSERT: we hold w->lock and parent_ff->lock

  START_INTERVAL(w, INTERVAL_FINALIZE_CHILD) {
    CILK_ASSERT(child_ff->is_call_child);
    CILK_ASSERT(child_ff->join_counter == 0);
    CILK_ASSERT(!child_ff->rightmost_child);
    CILK_ASSERT(child_ff == parent_ff->rightmost_child);

    // CHILD is about to die. 
    // Splicing out reducers is a no-op for a call since
    // w->reducer_map should already store the correct 
    // reducer map.

    // ASSERT there are no maps left to reduce.
    CILK_ASSERT(NULL == child_ff->children_reducer_map);
    CILK_ASSERT(NULL == child_ff->right_reducer_map);

    splice_exceptions_for_call(w, parent_ff, child_ff);

    splice_stacks_for_call(w, parent_ff, child_ff);

    /* remove CHILD from list of children of PARENT */
    unlink_child(parent_ff, child_ff);

    /* continue with the parent. */
    unconditional_steal(w, parent_ff);
    __cilkrts_destroy_full_frame(w, child_ff);
  } STOP_INTERVAL(w, INTERVAL_FINALIZE_CHILD);
}


/**
 * The invariant on ff->children_reducer_map is that when ff is
 * synched and when we are about to resume execution of ff, at least
 * one of ff->children_reducer_map and w->reducer_map must be NULL.
 *
 * Consider the two possibilities before resuming execution of ff:
 *
 * 1.  Suppose ff is synched and suspended.  Then either
 *
 *     (a) ff->children_reducer_map stores the reducer map that w
 *         should use, where w is the worker resuming execution of ff, 
 *         OR
 *     (b) w already has a user map, and ff->children_reducer_map is NULL. 
 *
 *     Case (a) happens when we are resuming execution of ff as a
 *     provably good steal.  In this case, w->reducer_map should be
 *     NULL and ff->children_reducer_map is valid.  To resume
 *     execution of ff on w, set w->reducer_map to
 *     ff->children_reducer_map.
 * 
 *     Case (b) occurs when we resume execution of ff because ff is a
 *     called child.  Then, ff->children_reducer_map should be NULL,
 *     and w should already have a valid reducer map when resuming
 *     execution of ff.  We resume execution of ff without changing
 *     w->reducer_map.
 *
 * 2. Suppose frame ff is not synched (i.e., it is active and might have
 *    active children).   Then ff->children_reducer_map is the slot for
 *    storing the reducer map from ff's leftmost child, as in the reducer
 *    protocol.   The runtime may resume execution of ff while it is not 
 *    synched only because of a steal.
 *    In this case, while we are resuming ff, ff->children_reducer_map
 *    may be non-NULL (because one of ff's children has completed).
 *    We resume execution of ff without changing w->reducer_map.
 */ 
static void setup_for_execution_reducers(__cilkrts_worker *w,
    full_frame *ff)
{
  // We only need to move ff->children_reducer_map into
  // w->reducer_map in case 1(a).
  //
  // First check whether ff is synched.
  __cilkrts_stack_frame *sf = ff->call_stack;
#ifdef CILK_IVARS
  //in the ivars variant, a self steal should never require that the reduce map be passed
  if (!(sf->flags & CILK_FRAME_UNSYNCHED) && ! (sf->flags & CILK_FRAME_SELF_STEAL)) {
#else
  if (!(sf->flags & CILK_FRAME_UNSYNCHED)) {
#endif
    // In this case, ff is synched. (Case 1).
      CILK_ASSERT(!ff->rightmost_child);

    // Test whether we are in case 1(a) and have
    // something to do.  Note that if both
    // ff->children_reducer_map and w->reducer_map are NULL, we
    // can't distinguish between cases 1(a) and 1(b) here.
    if (ff->children_reducer_map) {
      // We are in Case 1(a).
      CILK_ASSERT(!w->reducer_map);
      w->reducer_map = ff->children_reducer_map;
      ff->children_reducer_map = NULL;
    }
  }
}

static void setup_for_execution_exceptions(__cilkrts_worker *w, 
    full_frame *ff)
{
  CILK_ASSERT(NULL == w->l->pending_exception);
  w->l->pending_exception = ff->pending_exception;
  ff->pending_exception = NULL;
}

#if 0 /* unused */
static void setup_for_execution_stack(__cilkrts_worker *w, 
    full_frame *ff)
{
}
#endif

/*
 * setup_for_execution_pedigree
 *
 * Copies the pedigree information from the frame we're resuming to the
 * worker.  Increments the pedigree if this is work that has been stolen
 * to match the increment on a return from a spawn helper.
 */
static void setup_for_execution_pedigree(__cilkrts_worker *w)
{
  int pedigree_unsynched;
  __cilkrts_stack_frame *sf = w->current_stack_frame;

  CILK_ASSERT(NULL != sf);

  // If this isn't an ABI 1 or later frame, there's no pedigree information
  if (0 == CILK_FRAME_VERSION_VALUE(sf->flags))
    return;

  // Note whether the pedigree is unsynched and clear the flag before
  // we forget
  pedigree_unsynched = sf->flags & CILK_FRAME_SF_PEDIGREE_UNSYNCHED;
  sf->flags &= ~CILK_FRAME_SF_PEDIGREE_UNSYNCHED;

  // If we're just marshalling onto this worker, do not increment
  // the rank since that wouldn't happen in a sequential execution
  if (w->l->work_stolen || pedigree_unsynched)
  {
    if (w->l->work_stolen)
      w->pedigree.rank = sf->parent_pedigree.rank + 1;
    else
      w->pedigree.rank = sf->parent_pedigree.rank;
  }

  w->pedigree.parent = sf->parent_pedigree.parent;
  w->l->work_stolen = 0;
}

static void setup_for_execution(__cilkrts_worker *w, 
    full_frame *ff,
    int is_return_from_call)
{
  // ASSERT: We own w->lock and ff->lock || P == 1

  setup_for_execution_reducers(w, ff);
  setup_for_execution_exceptions(w, ff);
  /*setup_for_execution_stack(w, ff);*/

  ff->call_stack->worker = w;
  w->current_stack_frame = ff->call_stack;

  // If this is a return from a call, leave the pedigree alone
  if (! is_return_from_call)
    setup_for_execution_pedigree(w);

  __cilkrts_setup_for_execution_sysdep(w, ff);

  w->head = w->tail = w->l->ltq;
  reset_THE_exception(w);

  make_runnable(w, ff);
}

/* The current stack is about to either be suspended or destroyed.  This
 * function will switch to the stack on which the scheduler is suspended and
 * resume running the scheduler within function do_work().  Upon waking up,
 * the scheduler will run the 'cont' function, using the supplied worker and
 * frame.
 */
NORETURN longjmp_into_runtime(__cilkrts_worker *w,
    scheduling_stack_fcn_t fcn,
    __cilkrts_stack_frame *sf)
{
  full_frame *ff, *ff2;

  CILK_ASSERT(!w->l->post_suspend);
  ff = w->l->frame_ff;

  // If we've got only one worker, stealing shouldn't be possible.
  //
  // Assume that this is a steal or return from spawn in a force-reduce case.
  // We don't have a scheduling stack to switch to, so call the continuation
  // function directly.
#ifdef CILK_IVARS
  if (1 == w->g->P && !w->is_blocked) {
#else
    if (1 == w->g->P) {
#endif
      fcn(w, ff, sf);

      /* The call to function c() will have pushed ff as the next frame.  If
       * this were a normal (non-forced-reduce) execution, there would have
       * been a pop_next_frame call in a separate part of the runtime.  We
       * must call pop_next_frame here to complete the push/pop cycle. */
      ff2 = pop_next_frame(w);

      setup_for_execution(w, ff2, 0);
      __cilkrts_resume(w, ff2, w->current_stack_frame); /* no return */
      CILK_ASSERT(("returned from __cilkrts_resume", 0));
    }

    w->l->post_suspend = fcn;
    w->l->suspended_stack = sf;

    ITT_SYNC_RELEASING(w);
    ITT_SYNC_PREPARE(w);

    // If this is a user worker, and it's the first time that it's returned to
    // a stolen parent, we need to import the thread.  This will create a
    // scheduling stack or fiber, switch to that, and run the scheduling loop
    // on it
    if ((WORKER_USER == w->l->type) && (0 == w->l->user_thread_imported))
    {
      // We're importing the thread
      w->l->user_thread_imported = 1;
      __cilkrts_sysdep_import_user_thread(w);
      CILK_ASSERT(0); // Should never reach this point.
    }


#ifndef _WIN32

    // Jump to this thread's scheduling stack.
    longjmp(w->l->env, 1);
#else
    DBGPRINTF ("%d-%p: longjmp_into_runtime - "
        "Switching to scheduling fiber - %p\n"
        "           continuation routine: %p, sf: %p\n",
        w->self, GetWorkerFiber(w), w->sysdep->scheduling_fiber,
        fcn, sf);
#ifdef _DEBUG
    SetWorkerThreadName(w, NULL);
#endif
    SwitchToFiber(w->sysdep->scheduling_fiber);

    /* Since we switched away from the fiber on which this function was
     * entered, we will not get here until either the initial fiber is
     * resumed.  If the initial fiber belonged to a thief at a sync, then
     * the longjmp below will re-initialize the fiber for another steal.
     * If this fiber belonged to a victim, then the longjmp below will
     * resume the victim after the sync.
     */
    __cilkrts_resume_after_longjmp_into_runtime();
#endif
  }

  /*
   * Send a message to the children of the specified worker: run or wait.
   */
  static void notify_children(__cilkrts_worker *w, unsigned int msg)
  {
    int child_num;
    __cilkrts_worker *child;
    int num_sys_workers = w->g->P - 1;

    // If worker is "n", then its children are 2n + 1, and 2n + 2.
    child_num = (w->self << 1) + 1;
    if (child_num < num_sys_workers) {
      child = w->g->workers[child_num];
      CILK_ASSERT(child->l->signal_node);
      signal_node_msg(child->l->signal_node, msg);
      child_num++;
      if (child_num < num_sys_workers) {
        child = w->g->workers[child_num];
        CILK_ASSERT(child->l->signal_node);
        signal_node_msg(child->l->signal_node, msg);
      }
    }
  }

  /*
   * Notify this worker's children that they need to wait.
   */
  static void notify_children_wait(__cilkrts_worker *w)
  {
    notify_children(w, 0);
  }

  /*
   * Notify this worker's children to run and start trying to steal.
   */
  static void notify_children_run(__cilkrts_worker *w)
  {
    notify_children(w, 1);
  }

  //CSZ: experiencing stack corruption here. looks like the registers
  //are trashed on return from a paused computation. the w, and ff are completely gone. 
  //the actual frame is fine though, because if you make a volatile and copy the variables
  //down, they appear to be fine. the registers, however, remain completely corrupted in both 
  //cases. 
  static void do_work(__cilkrts_worker *w, full_frame *ff)
  {
    __cilkrts_stack_frame *sf;
    volatile short regs_restored = 0;

#ifndef _WIN32
    cilkbug_assert_no_uncaught_exception();
#endif

    BEGIN_WITH_WORKER_LOCK(w) {
      CILK_ASSERT(!w->l->frame_ff);
      BEGIN_WITH_FRAME_LOCK(w, ff) {
        sf = ff->call_stack;
#ifndef CILK_IVARS
        CILK_ASSERT(sf && !sf->call_parent);
#endif
        setup_for_execution(w, ff, 0);
      } END_WITH_FRAME_LOCK(w, ff);
    } END_WITH_WORKER_LOCK(w);

#if CILK_LIB_DEBUG
    if (!(sf->flags & CILK_FRAME_UNSYNCHED))
      CILK_ASSERT(!ff->stack_child);
    if (sf->flags & CILK_FRAME_EXITING) {
      __cilkrts_bug("W%d: resuming frame %p/%p suspended in exit\n",
          w->self, ff, sf);
    }
#endif

    /* run it */
    if (setjmp(w->l->env) == 0) {
      __cilkrts_resume(w, ff, sf);

      /* unreached---the call to cilk_resume exits through longjmp */
      CILK_ASSERT(0);
    }

    /* This point is reached for three reasons:

       1. Undo-detach finds parent stolen.

       2. Sync suspends frame.

       3. Return from Cilk entry point.

       In the first two cases the frame may be truly suspended or
       may be immediately executed by this worker after provably_good_steal.

       The active frame and call_stack may have changed since _resume.  */
    printf("hey! my continuation got called. in longejumped after resume\n");
    run_scheduling_stack_fcn(w);

    /* The worker borrowed the full frame's reducer map.
       Clear the extra reference.  Bookkeeping uses the
       copy in the frame, not the worker. */
    w->reducer_map = 0;

#ifndef _WIN32
    cilkbug_assert_no_uncaught_exception();
#endif
  }

#ifdef CILK_IVARS

  void self_steal(__cilkrts_worker *w)
  {
    full_frame *child_ff, *parent_ff = w->paused_ff;
    __cilkrts_stack_frame *sf;
    __cilkrts_stack *sd;
    char *sp;

    if(!can_steal_from(w)) return;

    CILK_ASSERT(w->l->frame_ff == 0);

    sd = __cilkrts_get_stack(w);
    if(NULL == sd) return;

    sf = __cilkrts_pop_tail(w);
    if(NULL == sf) return;

    sf->flags |= CILK_FRAME_SF_PEDIGREE_UNSYNCHED;
    child_ff = __cilkrts_make_full_frame(w, sf);
    child_ff->parent = parent_ff;
    child_ff->stack_self = sd;
    child_ff->sync_master = NULL;
    child_ff->is_call_child = 0;

    parent_ff->call_stack->flags |= CILK_FRAME_STOLEN;
    push_child(parent_ff, child_ff);
    sp = __cilkrts_stack_to_pointer(sd, sf);

    //looks like the last two arguments are never used? wtf...
    __cilkrts_bind_stack(child_ff, sp, NULL, NULL);

    sf->flags |= CILK_FRAME_SELF_STEAL;
    sf->call_parent = parent_ff->call_stack;
    make_unrunnable(w, child_ff, sf, 1, "self_steal");
    sf->flags &= ~CILK_FRAME_STOLEN;
    __cilkrts_push_next_frame(w,child_ff);
  }
#endif

  /*
   * Try to do work.  If there is none available, try to steal some and do it.
   */
  static void schedule_work(__cilkrts_worker *w)
  {
    full_frame *ff;

    ff = pop_next_frame(w);

    // If there is no work on the queue, try to steal some.
    if (NULL == ff) {

#ifdef CILK_IVARS

      //each worker must "pay their way" by checking
      //for a ready paused stack. Then they are free
      //to restore their own paused stack if it is ready.
      __cilkrts_paused_stack *pstk = NULL;

      dequeue(w->ready_queue, (ELEMENT_TYPE *) &pstk);
      if(pstk) {
        printf("dequeueing ctx %p\n", pstk);
        thaw_frame(pstk);
        CILK_ASSERT(0);
      }
#endif

      START_INTERVAL(w, INTERVAL_STEALING) {
        if (w->l->type != WORKER_USER && w->l->team != NULL) {
          // At this point, the worker knows for certain that it has run
          // out of work.  Therefore, it loses its team affiliation.  User
          // workers never change teams, of course.
          __cilkrts_worker_lock(w);
          w->l->team = NULL;
          __cilkrts_worker_unlock(w);
        }

#ifdef CILK_IVARS
        if(can_steal_from(w)) {
          self_steal(w);
        } else {
          random_steal(w);
        }
#else
        random_steal(w);
#endif

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

  static void __cilkrts_scheduler(__cilkrts_worker *w)
  {
    ITT_SYNC_PREPARE(w);

    START_INTERVAL(w, INTERVAL_IN_SCHEDULER) {

      /* this thread now becomes a worker---associate the thread
         with the worker state */
      __cilkrts_set_tls_worker(w);

      /* Notify tools about the new worker. Inspector needs this, but we
         don't want to confuse Cilkscreen with system threads.  User threads
         do this notification in bind_thread */
      if (! w->g->under_ptool)
        __cilkrts_cilkscreen_establish_worker(w);

      mysrand(w, (w->self + 1));

      if (WORKER_SYSTEM == w->l->type) {
        // Runtime begins in a wait-state and is woken up by the first user
        // worker when the runtime is ready.
        signal_node_wait(w->l->signal_node);
        // ...
        // Runtime is waking up.
        notify_children_run(w);
        w->l->steal_failure_count = 0;
      }

      while (!w->g->work_done) {

        switch (worker_runnable(w))
        {
          case SCHEDULE_RUN:             // do some work.
            schedule_work(w);
            break;

          case SCHEDULE_WAIT:            // go into wait-mode.
            CILK_ASSERT(WORKER_SYSTEM == w->l->type);
            notify_children_wait(w);
            signal_node_wait(w->l->signal_node);
            // ...
            // Runtime is waking up.
            notify_children_run(w);
            w->l->steal_failure_count = 0;
            break;

          case SCHEDULE_EXIT:            // exit the scheduler.
            CILK_ASSERT(WORKER_USER != w->l->type);
            break;

          default:
            CILK_ASSERT(0);
            abort();
        }

      } // while (!w->g->work_done)

    } STOP_INTERVAL(w, INTERVAL_IN_SCHEDULER);

    CILK_ASSERT(WORKER_SYSTEM == w->l->type);
  }


  /*************************************************************
    Forward declarations for reduction protocol.
   *************************************************************/

  static __cilkrts_worker*
    execute_reductions_for_sync(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *sf_at_sync);

  static __cilkrts_worker*
    execute_reductions_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *returning_sf);



  /*************************************************************
    Scheduler functions that are callable by client code
   *************************************************************/
  static full_frame *disown(__cilkrts_worker *w,
      full_frame *ff,
      __cilkrts_stack_frame *sf,
      const char *why)
  {
    CILK_ASSERT(ff);
    make_unrunnable(w, ff, sf, sf != 0, why);
    w->l->frame_ff = 0;
    return ff->parent;
  }

  /**
   * Called when ff is returning from a spawn, and we need to execute a
   * reduction.
   *
   * @param w             The currently executing worker.
   * @param ff            The full frame for w.
   * @param returning_sf  The stack frame for the spawn helper that is returning.
   *
   * Normally, by the time we gain control in the runtime, the worker
   * has already popped off the __cilkrts_stack_frame "returning_sf"
   * from its call chain.
   * 
   * When we have only serial reductions, w->current_stack_frame is not
   * needed any more, because w is about to enter the runtime scheduling
   * loop anyway.  Similarly, the frame "ff" is slated to be destroyed
   * after the runtime finishes the return from spawn and splices ff out
   * of the tree of full frames.
   *
   * To execute a parallel reduction, however, we still want
   * w->current_stack_frame == returning_sf, and we are going to use the
   * frame ff for a little bit longer.
   *
   * This method:
   *
   *   1. Puts returning_sf back as w's current stack frame.
   *   2. Makes "ff" runnable again on w.
   */ 
  static inline
    void restore_frame_for_spawn_return_reduction(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *returning_sf) {
#if REDPAR_DEBUG >= 2
      CILK_ASSERT(returning_sf);
      CILK_ASSERT(returning_sf->worker == w);
#endif
      // Change w's current stack frame back to "returning_sf".
      //
      // Intuitively, w->current_stack_frame should be
      // returning_sf->call_parent at this point.
      //
      // We can not assert this, however, because the pop of
      // returning_sf from the call chain has already cleared
      // returning_sf->call_parent.  We don't want to restore the call
      // parent of returning_sf, because its parent has been stolen, and
      // the runtime assumes that steals break this link.

      // We cannot assert call_parent is NULL either, since that's not true for
      // Win64 exception handling
      //    CILK_ASSERT(returning_sf->call_parent == NULL);
      w->current_stack_frame = returning_sf;

      // Make the full frame "ff" runnable again, in preparation for
      // executing the reduction.
      make_runnable(w, ff);
    }


  NORETURN __cilkrts_c_sync(__cilkrts_worker *w,
      __cilkrts_stack_frame *sf_at_sync)
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
    CILK_ASSERT(NULL == ff->pending_exception);
    ff->pending_exception = w->l->pending_exception;
    w->l->pending_exception = NULL;
#endif

    w = execute_reductions_for_sync(w, ff, sf_at_sync);

    longjmp_into_runtime(w, do_sync, sf_at_sync);
  }

  static void do_sync(__cilkrts_worker *w, full_frame *ff,
      __cilkrts_stack_frame *sf)
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

  /* worker W completely promotes its own deque, simulating the case
     where the whole deque is stolen.  We use this mechanism to force
     the allocation of new storage for reducers for race-detection
     purposes. */
  void __cilkrts_promote_own_deque(__cilkrts_worker *w)
  {
    BEGIN_WITH_WORKER_LOCK(w) {
      while (dekker_protocol(w)) {
        /* PLACEHOLDER_STACK is used as non-null marker to tell detach()
           and make_child() that this frame should be treated as a spawn
           parent, even though we have not assigned it a stack. */
        detach_for_steal(w, w, PLACEHOLDER_STACK);

      }
    } END_WITH_WORKER_LOCK(w);
  }



  /* the client code calls this function after a spawn when the dekker
     protocol fails.  The function may either return or longjmp
     into the rts

     This function takes in a "returning_sf" argument which corresponds
     to the __cilkrts_stack_frame that we are finishing (i.e., the
     argument to __cilkrts_leave_frame).
     */
  void __cilkrts_c_THE_exception_check(__cilkrts_worker *w, 
      __cilkrts_stack_frame *returning_sf)
  {
    full_frame *ff;
    int stolen_p;
    __cilkrts_stack_frame *saved_sf = NULL;
    START_INTERVAL(w, INTERVAL_THE_EXCEPTION_CHECK);

    BEGIN_WITH_WORKER_LOCK(w) {
      ff = w->l->frame_ff;
      CILK_ASSERT(ff);
      /* This code is called only upon a normal return and never
         upon an exceptional return.  Assert that this is the
         case. */
      CILK_ASSERT(!w->l->pending_exception);

      reset_THE_exception(w);
      stolen_p = !(w->head < (w->tail + 1)); /* +1 because tail was
                                                speculatively
                                                decremented by the
                                                compiled code */

      if (stolen_p) {
        /* XXX This will be charged to THE for accounting purposes */
        __cilkrts_save_exception_state(w, ff);

        // Save the value of the current stack frame.
        saved_sf = w->current_stack_frame;

        // Reverse the decrement from undo_detach.
        // This update effectively resets the deque to be
        // empty (i.e., changes w->tail back to equal w->head). 
        // We need to reset the deque to execute parallel
        // reductions.  When we have only serial reductions, it
        // does not matter, since serial reductions do not
        // change the deque.
        w->tail++;
#if REDPAR_DEBUG > 1            
        // ASSERT our deque is empty.
        CILK_ASSERT(w->head == w->tail);
#endif
      }
    } END_WITH_WORKER_LOCK(w);

    STOP_INTERVAL(w, INTERVAL_THE_EXCEPTION_CHECK);


    if (stolen_p)
    {
      w = execute_reductions_for_spawn_return(w, ff, returning_sf);

      // Update the pedigree only after we've finished the
      // reductions.
      update_pedigree_on_leave_frame(w, returning_sf);

#ifdef ENABLE_NOTIFY_ZC_INTRINSIC
      // Notify Inspector that the parent has been stolen and we're
      // going to abandon this work and go do something else.  This
      // will match the cilk_leave_begin in the compiled code
      __notify_zc_intrinsic("cilk_leave_stolen", saved_sf);
#endif // defined ENABLE_NOTIFY_ZC_INTRINSIC

      DBGPRINTF ("%d-%p: longjmp_into_runtime from __cilkrts_c_THE_exception_check\n", w->self, GetWorkerFiber(w));
      longjmp_into_runtime(w, do_return_from_spawn, 0);
      DBGPRINTF ("%d-%p: returned from longjmp_into_runtime from __cilkrts_c_THE_exception_check?!\n", w->self, GetWorkerFiber(w));
    }
    else
    {
      NOTE_INTERVAL(w, INTERVAL_THE_EXCEPTION_CHECK_USELESS);
      return;
    }
  }

  /* Return an exception to a stolen parent. */
  NORETURN __cilkrts_exception_from_spawn(__cilkrts_worker *w,
      __cilkrts_stack_frame *returning_sf) 
  {
    full_frame *ff = w->l->frame_ff;
    // This is almost the same as THE_exception_check, except
    // the detach didn't happen, we don't need to undo the tail
    // update.
    CILK_ASSERT(w->head == w->tail);
    w = execute_reductions_for_spawn_return(w, ff, returning_sf);

    longjmp_into_runtime(w, do_return_from_spawn, 0);
    CILK_ASSERT(0);
  }

   void do_return_from_spawn(__cilkrts_worker *w,
      full_frame *ff,
      __cilkrts_stack_frame *sf)
  {
    full_frame *parent_ff;
    BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) {
      CILK_ASSERT(ff);
      CILK_ASSERT(!ff->is_call_child);
      CILK_ASSERT(ff == w->l->frame_ff);
      CILK_ASSERT(sf == NULL);
      parent_ff = ff->parent;


      BEGIN_WITH_FRAME_LOCK(w, ff) {
        if( ff->stack_self )
        {
          // Notify TBB that we're returning from a spawn and orphaning
          // the stack. We'll re-adopt it if we continue
          __cilkrts_invoke_stack_op(w, CILK_TBB_STACK_ORPHAN,
              ff->stack_self);
        }
        decjoin(ff);
      } END_WITH_FRAME_LOCK(w, ff);

      BEGIN_WITH_FRAME_LOCK(w, parent_ff) {
        __cilkrts_stack* stack_to_free = w->l->stack_to_free;
        w->l->stack_to_free = NULL;
        w->l->frame_ff = NULL;

        if (stack_to_free) {
          __cilkrts_release_stack(w, stack_to_free);
        }
        ff->stack_self = NULL;

        if (parent_ff->simulated_stolen) {
          unconditional_steal(w, parent_ff);
        }
        else {
          provably_good_steal(w, parent_ff);
        }
      } END_WITH_FRAME_LOCK(w, parent_ff);

    } END_WITH_WORKER_LOCK_OPTIONAL(w);

    // Cleanup the child frame.
    __cilkrts_destroy_full_frame(w, ff);
    return;
  }

#ifdef _WIN32
  /* migrate an exception across fibers.  Call this function when an exception has
   * been thrown and has to traverse across a steal.  The exception has already
   * been wrapped up, so all that remains is to longjmp() into the continuation,
   * sync, and re-raise it.
   */
  void __cilkrts_migrate_exception(__cilkrts_stack_frame *sf) {

    __cilkrts_worker *w = sf->worker;
    full_frame *ff;

    BEGIN_WITH_WORKER_LOCK(w) {
      ff = w->l->frame_ff;
      reset_THE_exception(w);
      /* there is no need to check for a steal because we wouldn't be here if
         there weren't a steal. */
      __cilkrts_save_exception_state(w, ff);

      CILK_ASSERT(w->head == w->tail);
    } END_WITH_WORKER_LOCK(w);

    {
      // TBD(jsukha): This function emulates the
      // the "do_return_from_spawn" path.
      w = execute_reductions_for_spawn_return(w, ff, sf);
    }

    longjmp_into_runtime(w, do_return_from_spawn, 0); /* does not return. */
    CILK_ASSERT(! "Shouldn't be here...");
  }
#endif


  /* Pop a call stack from TAIL.  Return the call stack, or NULL if the
     queue is empty */
  __cilkrts_stack_frame *__cilkrts_pop_tail(__cilkrts_worker *w)
  {
    __cilkrts_stack_frame *sf;
    BEGIN_WITH_WORKER_LOCK(w) {
      __cilkrts_stack_frame *volatile *tail = w->tail;
      if (w->head < tail) {
        --tail;
        sf = *tail;
        w->tail = tail;
      } else {
        sf = 0;
      }
    } END_WITH_WORKER_LOCK(w);
    return sf;
  }

#ifdef CILK_IVARS

 void do_return_from_self (__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf)
 {
   return;
 }

static void blocked_frame_finalize_child_for_call(__cilkrts_worker *w,
    full_frame *parent_ff,
    full_frame *child_ff)
{
  // ASSERT: we hold w->lock and parent_ff->lock

  START_INTERVAL(w, INTERVAL_FINALIZE_CHILD) {
    //TODO TMP: make runtime happy. This isn't necessarily true in this case. 
    child_ff->is_call_child = 1;
    CILK_ASSERT(child_ff->join_counter == 0);
    CILK_ASSERT(!child_ff->rightmost_child);
    CILK_ASSERT(child_ff == parent_ff->rightmost_child);

    // CHILD is about to die. 
    // Splicing out reducers is a no-op for a call since
    // w->reducer_map should already store the correct 
    // reducer map.

    // ASSERT there are no maps left to reduce.
    CILK_ASSERT(NULL == child_ff->children_reducer_map);
    CILK_ASSERT(NULL == child_ff->right_reducer_map);

    splice_exceptions_for_call(w, parent_ff, child_ff);

    //we do not splice stacks. The child gets its own stack,
    //and the parent's stack is frozen. We can now reuse the child stack.

    CILK_ASSERT(child_ff->stack_self);
    __cilkrts_release_stack(w, child_ff->stack_self);

    /* remove CHILD from list of children of PARENT */
    unlink_child(parent_ff, child_ff);

    /* continue with the parent. */
    unconditional_steal(w, parent_ff);
    __cilkrts_destroy_full_frame(w, child_ff);
  } STOP_INTERVAL(w, INTERVAL_FINALIZE_CHILD);
}

  void self_steal_return(__cilkrts_worker *w) {
    full_frame *ff, *parent_ff;
    START_INTERVAL(w, INTERVAL_RETURNING);

    BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) {
      ff = w->l->frame_ff;
      CILK_ASSERT(ff);
      CILK_ASSERT(ff->join_counter == 1);

      BEGIN_WITH_FRAME_LOCK(w, ff) {
        // After this call, w->l->frame_ff != ff.
        // Technically, w will "own" ff until ff is freed,
        // however, because ff is a dying leaf full frame.
        parent_ff = disown(w, ff, 0, "return");

        decjoin(ff);

#ifdef _WIN32
        __cilkrts_save_exception_state(w, ff);
#else
        // Move the pending exceptions into the full frame
        // This should always be NULL if this isn't a
        // return with an exception
        CILK_ASSERT(NULL == ff->pending_exception);
        ff->pending_exception = w->l->pending_exception;
        w->l->pending_exception = NULL;
#endif  // _WIN32

      } END_WITH_FRAME_LOCK(w, ff);

      __cilkrts_fence(); /* redundant */

      CILK_ASSERT(parent_ff);

      BEGIN_WITH_FRAME_LOCK(w, parent_ff) {
        w->l->frame_ff = 0;
        if(parent_ff->call_stack->flags & CILK_FRAME_BLOCKED) {
          blocked_frame_finalize_child_for_call(w, parent_ff, ff);
        } else {
          finalize_child_for_call(w, parent_ff, ff);
        }
      } END_WITH_FRAME_LOCK(w, parent_ff);

      ff = pop_next_frame(w);
      /* ff will be non-null except when the parent frame is owned
         by another worker.
         CILK_ASSERT(ff)
         */
      CILK_ASSERT(!w->l->frame_ff);
      if (ff) {
        BEGIN_WITH_FRAME_LOCK(w, ff) {
          __cilkrts_stack_frame *sf = ff->call_stack;
          CILK_ASSERT(sf && (!sf->call_parent || w->is_blocked));
          //dirty little hack to workaround the reducer's view 
          //of ivars doing a non nested return pattern.
          sf->flags |=CILK_FRAME_SELF_STEAL;
          setup_for_execution(w, ff, 1);
          sf->flags &= ~CILK_FRAME_SELF_STEAL;
        } END_WITH_FRAME_LOCK(w, ff);
      }
    } END_WITH_WORKER_LOCK_OPTIONAL(w);

    STOP_INTERVAL(w, INTERVAL_RETURNING);
  }
#endif

  /* Return from a call, not a spawn. */
  void __cilkrts_return(__cilkrts_worker *w)
  {
    full_frame *ff, *parent_ff;
    START_INTERVAL(w, INTERVAL_RETURNING);

    BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) {
      ff = w->l->frame_ff;
      CILK_ASSERT(ff);
      CILK_ASSERT(ff->join_counter == 1);
      /* This path is not used to return from spawn. */
      CILK_ASSERT(ff->is_call_child);

      BEGIN_WITH_FRAME_LOCK(w, ff) {
        // After this call, w->l->frame_ff != ff.
        // Technically, w will "own" ff until ff is freed,
        // however, because ff is a dying leaf full frame.
        parent_ff = disown(w, ff, 0, "return");
        decjoin(ff);

#ifdef _WIN32
        __cilkrts_save_exception_state(w, ff);
#else
        // Move the pending exceptions into the full frame
        // This should always be NULL if this isn't a
        // return with an exception
        CILK_ASSERT(NULL == ff->pending_exception);
        ff->pending_exception = w->l->pending_exception;
        w->l->pending_exception = NULL;
#endif  // _WIN32

      } END_WITH_FRAME_LOCK(w, ff);

      __cilkrts_fence(); /* redundant */

      CILK_ASSERT(parent_ff);

      BEGIN_WITH_FRAME_LOCK(w, parent_ff) {
        finalize_child_for_call(w, parent_ff, ff);
      } END_WITH_FRAME_LOCK(w, parent_ff);

      ff = pop_next_frame(w);
      /* ff will be non-null except when the parent frame is owned
         by another worker.
         CILK_ASSERT(ff)
         */
      CILK_ASSERT(!w->l->frame_ff);
      if (ff) {
        BEGIN_WITH_FRAME_LOCK(w, ff) {
          __cilkrts_stack_frame *sf = ff->call_stack;
          CILK_ASSERT(sf && !sf->call_parent);
          setup_for_execution(w, ff, 1);
        } END_WITH_FRAME_LOCK(w, ff);
      }
    } END_WITH_WORKER_LOCK_OPTIONAL(w);

    STOP_INTERVAL(w, INTERVAL_RETURNING);
  }

  static void __cilkrts_unbind_thread()
  {
    int stop_cilkscreen = 0;
    global_state_t *g;

    // Take out the global OS mutex to protect accesses to the table of workers
    global_os_mutex_lock();

    if (cilkg_is_published()) {
      __cilkrts_worker *w = __cilkrts_get_tls_worker();
      if (w) {
        g = w->g;

        // If there's only 1 worker, the counts will be stopped in
        // __cilkrts_scheduler
        if (g->P > 1)
        {
          STOP_INTERVAL(w, INTERVAL_WORKING);
          STOP_INTERVAL(w, INTERVAL_IN_SCHEDULER);
        }

        __cilkrts_sysdep_unbind_thread(w);
        __cilkrts_set_tls_worker(0);

        if (w->self == -1) {
          // This worker is an overflow worker.  I.e., it was created on-
          // demand when the global pool ran out of workers.
          destroy_worker(w);
          __cilkrts_free(w);
        } else {
          // This is a normal user worker and needs to be counted by the
          // global state for the purposes of throttling system workers.
          w->l->type = WORKER_FREE;
          __cilkrts_leave_cilk(g);
        }

        stop_cilkscreen = (0 == g->Q);
      }
    }
    global_os_mutex_unlock();

    /* Turn off Cilkscreen.  This needs to be done when we are NOT holding the
     * os mutex. */
    if (stop_cilkscreen)
      __cilkrts_cilkscreen_disable_instrumentation();
  }

  /* special return from the initial frame */

  void __cilkrts_c_return_from_initial(__cilkrts_worker *w)
  {
    struct cilkred_map *rm;

    /* This is only called on a user thread worker. */
    CILK_ASSERT(w->l->type == WORKER_USER);

#if REDPAR_DEBUG >= 3
    fprintf(stderr, "[W=%d, desc=cilkrts_c_return_from_initial, ff=%p]\n",
        w->self, w->l->frame_ff);
#endif

    BEGIN_WITH_WORKER_LOCK_OPTIONAL(w) {
      full_frame *ff = w->l->frame_ff;
      CILK_ASSERT(ff);
      printf("ff->join_counter %d\n", ff->join_counter);
      CILK_ASSERT(ff->join_counter == 1);
      w->l->frame_ff = 0;

      CILK_ASSERT(ff->stack_self);
      // Save any TBB interop data for the next time this thread enters Cilk
      tbb_interop_save_info_from_stack(ff->stack_self);
      sysdep_destroy_user_stack(ff->stack_self);

      /* Save reducer map into global_state object */
      rm = w->reducer_map;
      w->reducer_map = NULL;

#if REDPAR_DEBUG >= 3
      fprintf(stderr, "W=%d, reducer_map_to_delete=%p, was in ff=%p\n",
          w->self,
          rm,
          ff);
#endif
      __cilkrts_destroy_full_frame(w, ff);


      /* Work is never done. w->g->work_done = 1; __cilkrts_fence(); */
    } END_WITH_WORKER_LOCK_OPTIONAL(w);


    save_pedigree_leaf_from_user_worker(w);

    // Workers can have NULL reducer maps now.
    if (rm) {
      __cilkrts_destroy_reducer_map(w, rm);
    }

    w = NULL;
    __cilkrts_unbind_thread();

    /* Other workers will stop trying to steal if this was the last worker. */

    return;
  }


  /*
   * __cilkrts_restore_stealing
   *
   * Restore the protected_tail to a previous state, possibly allowing frames
   * to be stolen.  The dekker_protocol has been extended to steal only if
   * head+1 is < protected_tail.
   */

  void __cilkrts_restore_stealing(
      __cilkrts_worker *w,
      __cilkrts_stack_frame *volatile *saved_protected_tail)
  {
    /* On most x86 this pair of operations would be slightly faster
       as an atomic exchange due to the implicit memory barrier in
       an atomic instruction. */
    w->protected_tail = saved_protected_tail;
    __cilkrts_fence();
  }

  /*
   * __cilkrts_disallow_stealing
   *
   * Move the protected_tail to NEW_PROTECTED_TAIL, preventing any
   * frames from being stolen.  If NEW_PROTECTED_TAIL is NULL, prevent
   * stealing from the whole queue.  The dekker_protocol has been
   * extended to only steal if head+1 is also < protected_tail.
   */

  __cilkrts_stack_frame *volatile *__cilkrts_disallow_stealing(
      __cilkrts_worker *w,
      __cilkrts_stack_frame *volatile *new_protected_tail)
  {
    __cilkrts_stack_frame *volatile *saved_protected_tail = w->protected_tail;

    if (!new_protected_tail)
      new_protected_tail = w->l->ltq;

    if (w->protected_tail > new_protected_tail) {
      w->protected_tail = new_protected_tail;
      /* Issue a store-store barrier.  The update to protected_tail
         here must precede the update to tail in the next spawn.
         On x86 this is probably not needed. */
#if defined __GNUC__ && __ICC >= 1200 && !(__MIC__ ||__MIC2__)
      _mm_sfence();
#else
      __cilkrts_fence();
#endif
    }

    return saved_protected_tail;
  }

  /*************************************************************
    Initialization and startup 
   *************************************************************/

  __cilkrts_worker *make_worker(global_state_t *g,
      int self, __cilkrts_worker *w)
  {
    w->self = self;
    w->g = g;

    w->pedigree.rank = 0;    // Initial rank is 0
    w->pedigree.parent = NULL;

    w->l = (local_state *)__cilkrts_malloc(sizeof(*w->l));

    __cilkrts_init_stats(&w->l->stats);

    __cilkrts_frame_malloc_per_worker_init(w);

    w->l->worker_magic_0 = WORKER_MAGIC_0;
    __cilkrts_mutex_init(&w->l->lock);
    __cilkrts_mutex_init(&w->l->steal_lock);
    w->l->do_not_steal = 0;
    w->l->frame_ff = 0;
    w->l->ltq = (__cilkrts_stack_frame **)
      __cilkrts_malloc(g->ltqsize * sizeof(*w->l->ltq));
    w->ltq_limit = w->l->ltq + g->ltqsize;

    w->l->original_pedigree_leaf = NULL;

    w->l->rand_seed = 0; /* the scheduler will overwrite this field */
    w->l->next_frame_ff = 0;
    __cilkrts_init_stack_cache(w, &w->l->stack_cache, g->stack_cache_size);

    w->head = w->tail = w->l->ltq;

    w->reducer_map = NULL;

    w->current_stack_frame = NULL;

    w->l->pending_exception = NULL;
    w->l->worker_magic_1 = WORKER_MAGIC_1;

    w->l->post_suspend = 0;
    w->l->suspended_stack = 0;
    w->l->stack_to_free = NULL;

    w->l->steal_failure_count = 0;

    w->l->team = NULL;
    w->l->last_full_frame = NULL;

    w->l->scheduler_stack = NULL;

    w->l->signal_node = NULL;

    w->reserved = NULL;
    /*w->parallelism_disabled = 0;*/

    // Allow stealing all frames. Sets w->saved_protected_tail
    __cilkrts_restore_stealing(w, w->ltq_limit);

    w->l->type = WORKER_FREE;
    w->l->user_thread_imported = 0;

    // Nothing's been stolen yet
    w->l->work_stolen = 0;

    __cilkrts_init_worker_sysdep(w);

    reset_THE_exception(w);

#ifdef CILK_IVARS
        if(w->self != -1)
          w->worker_cache = (queue_t *) make_stack_queue();
        w->is_replacement = 0;
        w->ivar = NULL;
        memset(w->ctx, 0, 5);

        //register our own worker in the stealing array
        //--------------------
        w->forwarding_array = init_array();  /* If I have no work to steal, you can follow this link. */
        BEGIN_WITH_FORWARDING_ARRAY_LOCK(w->forwarding_array) {
          w->forwarding_array->ptrs[ARRAY_SIZE-1] = w;
          w->forwarding_array->elems++;
          w->forwarding_array->leftmost_idx = ARRAY_SIZE-1;
        } END_WITH_FORWARDING_ARRAY_LOCK(w->forwarding_array);
        //--------------------
#endif

    return w;
  }

  void destroy_worker(__cilkrts_worker *w)
  {
    CILK_ASSERT (NULL == w->l->pending_exception);

    /* Free any cached stack. */
    __cilkrts_destroy_stack_cache(w, w->g, &w->l->stack_cache);

    if (w->l->scheduler_stack) {
      sysdep_destroy_tiny_stack(w->l->scheduler_stack);
      w->l->scheduler_stack = NULL;
    }
    __cilkrts_destroy_worker_sysdep(w);

    if (w->l->signal_node) {
      CILK_ASSERT(WORKER_SYSTEM == w->l->type);
      signal_node_destroy(w->l->signal_node);
    }

    __cilkrts_free(w->l->ltq);
    __cilkrts_mutex_destroy(0, &w->l->lock);
    __cilkrts_mutex_destroy(0, &w->l->steal_lock);
    __cilkrts_frame_malloc_per_worker_cleanup(w);
    __cilkrts_free(w->l);

    // The caller is responsible for freeing the worker memory
  }

  /*
   * Make a worker into a system worker.
   */
  static void make_worker_system(__cilkrts_worker *w) {
    CILK_ASSERT(WORKER_FREE == w->l->type);
    w->l->type = WORKER_SYSTEM;
    w->l->signal_node = signal_node_create();
  }

  void __cilkrts_deinit_internal(global_state_t *g)
  {
    int i;
    __cilkrts_worker *w;

    // If there's no global state then we're done
    if (NULL == g)
      return;

#ifdef CILK_PROFILE
    __cilkrts_dump_stats_to_stderr(g);
#endif

    w = g->workers[0];
    if (w->l->frame_ff) {
      __cilkrts_destroy_full_frame(w, w->l->frame_ff);
      w->l->frame_ff = 0;
    }

    // Destroy any system dependent global state
    __cilkrts_destroy_global_sysdep(g);

    for (i = 0; i < g->total_workers; ++i)
      destroy_worker(g->workers[i]);

    // Free memory for all worker blocks which were allocated contiguously
    __cilkrts_free(g->workers[0]);

    __cilkrts_free(g->workers);
    __cilkrts_destroy_stack_cache(0, g, &g->stack_cache);
    __cilkrts_frame_malloc_global_cleanup(g);
    cilkg_deinit_global_state();
  }

  /*
   * Wake the runtime by notifying the system workers that they can steal.  The
   * first user worker into the runtime should call this.
   */
  static void wake_runtime(global_state_t *g)
  {
    __cilkrts_worker *root;
    if (g->P > 1) {
      // Send a message to the root node.  The message will propagate.
      root = g->workers[0];
      CILK_ASSERT(root->l->signal_node);
      signal_node_msg(root->l->signal_node, 1);
    }
  }

  /*
   * Put the runtime to sleep.  The last user worker out of the runtime should
   * call this.  Like Dad always said, turn out the lights when nobody's in the
   * room.
   */
  static void sleep_runtime(global_state_t *g)
  {
    __cilkrts_worker *root;
    if (g->P > 1) {
      // Send a message to the root node.  The message will propagate.
      root = g->workers[0];
      CILK_ASSERT(root->l->signal_node);
      signal_node_msg(root->l->signal_node, 0);
    }
  }

  /* Called when a user thread joins Cilk.
     Global lock must be held. */
  void __cilkrts_enter_cilk(global_state_t *g)
  {
    if (g->Q++ == 0) {
      // If this is the first user thread to enter Cilk wake
      // up all the workers.
      wake_runtime(g);
    }
  }

  /* Called when a user thread leaves Cilk.
     Global lock must be held. */
  void __cilkrts_leave_cilk(global_state_t *g)
  {
    if (--g->Q == 0) {
      // Put the runtime to sleep.
      sleep_runtime(g);
    }
  }

  /*
   * worker_runnable
   *
   * Return true if the worker should continue to try to steal.  False, otherwise.
   */

  NOINLINE
    static enum schedule_t worker_runnable(__cilkrts_worker *w)
    {
      global_state_t *g = w->g;

      /* If this worker has something to do, do it.
         Otherwise the work would be lost. */
      if (w->l->next_frame_ff)
        return SCHEDULE_RUN;

      // If Cilk has explicitly (by the user) been told to exit (i.e., by
      // __cilkrts_end_cilk() -> __cilkrts_stop_workers(g)), then return 0.
      if (g->work_done)
        return SCHEDULE_EXIT;

      if (0 == w->self) {
        // This worker is the root node and is the only one that may query the
        // global state to see if there are still any user workers in Cilk.
        if (w->l->steal_failure_count > g->max_steal_failures) {
          if (signal_node_should_wait(w->l->signal_node)) {
            return SCHEDULE_WAIT;
          } else {
            // Reset the steal_failure_count since we have verified that
            // user workers are still in Cilk.
            w->l->steal_failure_count = 0;
          }
        }
      } else if (WORKER_SYSTEM == w->l->type &&
          signal_node_should_wait(w->l->signal_node)) {
        // This worker has been notified by its parent that it should stop
        // trying to steal.
        return SCHEDULE_WAIT;
      }

      return SCHEDULE_RUN;
    }

  // Initialize the worker structs, but don't start the workers themselves.
  static void init_workers(global_state_t *g)
  {
    int total_workers = g->total_workers;
    int i;
    struct CILK_ALIGNAS(256) buffered_worker {
      __cilkrts_worker w;
      char buf[64];
    } *workers_memory;

    /* not needed if only one worker */
    __cilkrts_init_stack_cache(0, &g->stack_cache,
        2*total_workers * g->global_stack_cache_size);

    g->workers = (__cilkrts_worker **)
      __cilkrts_malloc(total_workers * sizeof(*g->workers));

    // Allocate 1 block of memory for workers to make life easier for tools
    // like Inspector which run multithreaded and need to know the memory
    // range for all the workers that will be accessed in a user's program
    workers_memory = (struct buffered_worker*)
      __cilkrts_malloc(sizeof(*workers_memory) * total_workers);    

    // Notify any tools that care (Cilkscreen and Inspector) that they should
    // ignore memory allocated for the workers
    __cilkrts_cilkscreen_ignore_block(&workers_memory[0],
        &workers_memory[total_workers]);

    // Initialize worker structs, including unused worker slots.
    for (i = 0; i < total_workers; ++i) {
      g->workers[i] = make_worker(g, i, &workers_memory[i].w);
    }

    // Set the workers in the first P - 1 slots to be system workers.
    // Remaining worker structs already have type == 0.
    for (i = 0; i < g->system_workers; ++i) {
      make_worker_system(g->workers[i]);
    }
  }

  void __cilkrts_init_internal(int start)
  {
    int i;
    global_state_t *g = NULL;

    if (cilkg_is_published()) {
      g = cilkg_init_global_state();
    }
    else {

      // We think the state has not been published yet.
      // Grab the lock and try to initialize/publish.
      global_os_mutex_lock();

      if (cilkg_is_published()) {
        // Some other thread must have snuck in and published.
        g = cilkg_init_global_state();
      }
      else {
        // Initialize and retrieve global state
        g = cilkg_init_global_state();

        // Set the scheduler pointer
        g->scheduler = &__cilkrts_scheduler;

        // If we're running under a sequential P-Tool (Cilkscreen or
        // Cilkview) then there's only one worker and we need to tell
        // the tool about the extent of the stack
        if (g->under_ptool)
          __cilkrts_establish_c_stack();     
        init_workers(g);

        // Initialize any system dependent global state
        __cilkrts_init_global_sysdep(g);

        cilkg_publish_global_state(g);
      }

      global_os_mutex_unlock();
    }

    CILK_ASSERT(g);

    if (start && !g->workers_running)
    {
      // Acquire the global OS mutex while we're starting the workers
      global_os_mutex_lock();
      if (!g->workers_running)
        // Start P - 1 system workers since P includes the first user
        // worker.
        __cilkrts_start_workers(g, g->P - 1);
      global_os_mutex_unlock();
    }
  }


  /************************************************************************
    Methods for reducer protocol.

    Reductions occur in two places:
    A. A full frame "ff" is returning from a spawn with a stolen parent.
    B. A full frame "ff" is stalling at a sync.

    To support parallel reductions, reduction functions need to be
    executed while control is on a user stack, before jumping into the
    runtime.  These reductions can not occur while holding a worker or
    frame lock.

    Before a worker w executes a reduction in either Case A or B, w's
    deque is empty.

    Since parallel reductions push work onto the deque, we must do extra
    work to set up runtime data structures properly before reductions
    begin to allow stealing.  ( Normally, when we have only serial
    reductions, once a worker w starts a reduction, its deque remains
    empty until w either steals another frame or resumes a suspended
    frame.  Thus, we don't care about the state of the deque, since w
    will reset its deque when setting up execution of a frame. )

    To allow for parallel reductions, we coerce the runtime data
    structures so that, from their perspective, it looks as though we
    have spliced in an "execute_reductions()" function.  Consider the
    two cases for reductions:

    Case A: Return from a spawn with a stolen parent.
    Consider a spawned function g is returning on a worker w.
Assume:
-   g was spawned from a parent function f.  
-   ff is the full frame for g's spawn helper
-   sf be the __cilkrts_stack_frame for g's spawn helper.

We are conceptually splicing "execute_reductions()" so that it
occurs immediately before the spawn helper of g returns to f.

We do so by creating two different world views --- one for the
runtime data structures, and one for the actual control flow.

- Before reductions begin, the runtime data structures should
look as though the spawn helper of g is calling
"execute_reductions()", in terms of both the user stack and
worker deque.  More precisely, w should satisfy the
following properties:

(a) w has ff as its full frame,
(b) w has sf as its __cilkrts_stack_frame, and
(c) w has an empty deque. 

If the runtime satisfies these properties, then if w
encounters a spawn in a parallel reduction, it can push onto
a valid deque.  Also, when a steal from w occurs, it will
build the correct tree of full frames when w is stolen from.

- In actual control flow, however, once the
"execute_reductions()" function returns, it is actually
returning to runtime code instead of g's spawn helper. 

At the point a worker w began executing reductions, the
control flow / compiled code had already finished g's spawn
helper, and w was about to enter the runtime.  With parallel
reductions, some worker v (which might be different from w)
is the one returning to the runtime.


The reduction logic consists of 4 steps:

A1. Restore runtime data structures to make it look as though
the spawn helper of g() is still the currently executing
  frame for w.

    A2. Execute reductions on the user stack.  Reductions also
    includes the logic for exceptions and stacks.  Note that
    reductions start on w, but may finish on a different
    worker if there is parallelism in the reduce.

    A3. Splice out ff from the tree of full frames.

    A4. Jump into the runtime/scheduling stack and execute
    "do_return_from_spawn".  This method

    (a) Frees the user stack we were just on if it is no longer needed.
    (b) Decrement the join counter on ff->parent, and tries to do a
    provably good steal.
    (c) Clean up the full frame ff. 


    Case B: Stalling at a sync.

    Consider a function g(), with full frame ff and
    __cilkrts_stack_frame sf.  Suppose g() stalls at a sync, and we
    are executing reductions.

    Conceptually, we are splicing in an "execute_reductions()"
    function into g() as the last action that g() takes immediately
    before it executes the cilk_sync.

    The reduction logic for this case is similar to Case A.

    B1. Restore the runtime data structures. 

    The main difference from Case A is that ff/sf is still a
    frame that needs to be executed later (since it is stalling
        at a cilk_sync).  Thus, we also need to save the current
    stack information into "ff" so that we can correctly resume
    execution of "ff" after the sync.

    B2. Execute reductions on the user stack.

    B3. No frame to splice out of the tree.

    B4. Jump into the runtime/scheduling stack and execute "do_sync".
    This method:
    (a) Frees the user stack we were just on if it is no longer needed.
    (b) Tries to execute a provably good steal.

    Finally, for the reducer protocol, we consider two reduction paths,
    namely a "fast" and "slow" path.  On a fast path, only trivial
      merges of reducer maps happen (i.e., one or both of the maps are
          NULL).  Otherwise, on the slow path, a reduction actually needs to
      happen.

      *****************************************************************/

      // Struct storing pointers to the fields in our "left" sibling
      // that we should update when splicing out a full frame or stalling at
      // a sync.
      typedef struct {
        // A pointer to the location of our left reducer map. 
        struct cilkred_map **map_ptr;

        // A pointer to the location of our left exception.
        struct pending_exception_info **exception_ptr;
      } splice_left_ptrs;

  /**
   * For a full frame returning from a spawn, calculate the pointers to
   * the maps and exceptions to my left.
   *
   * @param w   The currently executing worker.
   * @param ff  Full frame that is dying
   * @return    Pointers to our "left" for reducers and exceptions.
   */
  static inline
    splice_left_ptrs compute_left_ptrs_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff)
    {
      // ASSERT: we hold the lock on ff->parent

      splice_left_ptrs left_ptrs;
      if (ff->left_sibling) {
        left_ptrs.map_ptr = &ff->left_sibling->right_reducer_map;
        left_ptrs.exception_ptr = &ff->left_sibling->right_pending_exception;
      }
      else {
        full_frame *parent_ff = ff->parent;
        left_ptrs.map_ptr = &parent_ff->children_reducer_map;
        left_ptrs.exception_ptr = &parent_ff->child_pending_exception;
      }
      return left_ptrs;
    }

  /**
   * For a full frame at a sync, calculate the pointers to the maps and
   * exceptions to my left.
   *
   * @param w   The currently executing worker.
   * @param ff  Full frame that is stalling at a sync.
   * @return    Pointers to our "left" for reducers and exceptions.
   */
  static inline
    splice_left_ptrs compute_left_ptrs_for_sync(__cilkrts_worker *w,
        full_frame *ff)
    {
      // ASSERT: we hold the lock on ff
      splice_left_ptrs left_ptrs;

      // Figure out which map to the left we should merge into.
      if (ff->rightmost_child) {
        CILK_ASSERT(ff->rightmost_child->parent == ff);
        left_ptrs.map_ptr = &(ff->rightmost_child->right_reducer_map);
        left_ptrs.exception_ptr = &(ff->rightmost_child->right_pending_exception);
      }
      else {
        // We have no children.  Then, we should be the last
        // worker at the sync... "left" is our child map.
        left_ptrs.map_ptr = &(ff->children_reducer_map);
        left_ptrs.exception_ptr = &(ff->child_pending_exception);
      }
      return left_ptrs;
    }

  /**
   * After we have completed all reductions on a spawn return, call this
   * method to finish up before jumping into the runtime.
   *
   *   1. Perform the "reduction" on stacks, i.e., execute the left
   *      holder logic to pass the leftmost stack up.
   *
   *      w->l->stack_to_free holds any stack that needs to be freed
   *      after control longjmps into the runtime.
   * 
   *   2. Unlink and remove child_ff from the tree of full frames.
   *
   * @param   w          The currently executing worker.
   * @param   parent_ff  The parent of child_ff.
   * @param   child_ff   The full frame returning from a spawn.
   */
  static inline
    void finish_spawn_return_on_user_stack(__cilkrts_worker *w,
        full_frame *parent_ff,
        full_frame *child_ff)
    {
      CILK_ASSERT(w->l->stack_to_free == NULL);

      // Execute left-holder logic for stacks.
      if (child_ff->left_sibling || parent_ff->stack_child) {
        // Case where we are not the leftmost stack.
        CILK_ASSERT(parent_ff->stack_child != child_ff->stack_self);

        // Remember any stack we need to free in the worker.
        // After we jump into the runtime, we will actually do the
        // free.
        w->l->stack_to_free = child_ff->stack_self;
      }
      else {
        // We are leftmost, pass stack up to parent.
        // Thus, no stack to free.
        parent_ff->stack_child = child_ff->stack_self;
        w->l->stack_to_free = NULL;
      }

      // We cannot NULL this out yet.  Importing a user worker on Windows
      // depends on this field in the full_frame being valid in
      // __cilkrts_sysdep_import_user_thread()
      //    child_ff->stack_self = NULL;

      unlink_child(parent_ff, child_ff);
    }


  /**
   * Executes any fast reductions necessary to splice ff out of the tree
   * of full frames.
   *
   * This "fast" path performs only trivial merges of reducer maps,
   * i.e,. when one of them is NULL.
   * (See slow_path_reductions_for_spawn_return() for slow path.)
   *
   * Returns: 1 if we finished all reductions.
   * Returns: 0 if there are still reductions to execute, and
   *            we should execute the slow path.
   *
   * This method assumes w holds the frame lock on parent_ff.
   * After this method completes:
   *    1. We have spliced ff out of the tree of full frames.
   *    2. The reducer maps of child_ff have been deposited
   *       "left" according to the reducer protocol.
   *    3. w->l->stack_to_free stores the stack
   *       that needs to be freed once we jump into the runtime.
   *
   * We have not, however, decremented the join counter on ff->parent.
   * This prevents any other workers from resuming execution of the parent.
   *
   * @param   w    The currently executing worker.
   * @param   ff   The full frame returning from a spawn.
   * @return  NULL if we finished all reductions.
   * @return  The address where the left map is stored (which should be passed to 
   *          slow_path_reductions_for_spawn_return()) if there are
   *          still reductions to execute. 
   */
  struct cilkred_map**
    fast_path_reductions_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff)
    {
      // ASSERT: we hold ff->parent->lock.
      full_frame *parent_ff = ff->parent;
      splice_left_ptrs left_ptrs;

      CILK_ASSERT(NULL == w->l->pending_exception);

      // Figure out the pointers to the left where I want
      // to put reducers and exceptions.
      left_ptrs = compute_left_ptrs_for_spawn_return(w, ff);

      // Go ahead and merge exceptions while holding the lock.
      splice_exceptions_for_spawn(w, ff, left_ptrs.exception_ptr);

      // Now check if we have any reductions to perform.
      //
      // Consider all the cases of left, middle and right maps.
      //  0. (-, -, -)  :  finish and return 1
      //  1. (L, -, -)  :  finish and return 1
      //  2. (-, M, -)  :  slide over to left, finish, and return 1.
      //  3. (L, M, -)  :  return 0
      //  4. (-, -, R)  :  slide over to left, finish, and return 1.
      //  5. (L, -, R)  :  return 0
      //  6. (-, M, R)  :  return 0
      //  7. (L, M, R)  :  return 0
      //
      // In terms of code:
      //  L == *left_ptrs.map_ptr
      //  M == w->reducer_map
      //  R == f->right_reducer_map.
      //
      // The goal of the code below is to execute the fast path with
      // as few branches and writes as possible.

      int case_value = (*(left_ptrs.map_ptr) != NULL);
      case_value += ((w->reducer_map != NULL) << 1);
      case_value += ((ff->right_reducer_map != NULL) << 2);

      // Fastest path is case_value == 0 or 1.
      if (case_value >=2) {
        switch (case_value) {
          case 2:
            *(left_ptrs.map_ptr) = w->reducer_map;
            w->reducer_map = NULL;
            return NULL;
            break;
          case 4:
            *(left_ptrs.map_ptr) = ff->right_reducer_map;
            ff->right_reducer_map = NULL;
            return NULL;
          default:
            // If we have to execute the slow path, then
            // return the pointer to the place to deposit the left
            // map.
            return left_ptrs.map_ptr;
        }
      }

      // Do nothing
      return NULL;
    }


  /**
   * Executes any reductions necessary to splice "ff" frame out of
   * the steal tree.
   *
   * This method executes the "slow" path for reductions on a spawn
   * return, i.e., there are non-NULL maps that need to be merged
   * together.
   *
   * This method should execute only if
   * fast_path_reductions_for_spawn_return() returns a non-NULL
   * left_map_ptr.
   *
   * Upon entry, left_map_ptr should be the location of the left map
   * at the start of the reduction, as calculated by
   * fast_path_reductions_for_spawn_return().
   *
   * After this method completes:
   *    1. We have spliced ff out of the tree of full frames.
   *    2. The reducer maps of child_ff have been deposited
   *       "left" according to the reducer protocol.
   *    3. w->l->stack_to_free stores the stack
   *       that needs to be freed once we jump into the runtime.
   * We have not, however, decremented the join counter on ff->parent,
   * so no one can resume execution of the parent yet.
   *
   * WARNING: 
   *   This method assumes the lock on ff->parent is held upon entry, and
   *   Upon exit, the worker that returns still holds a lock on ff->parent
   *   This method can, however, release and reacquire the lock on ff->parent.
   *
   * @param w             The currently executing worker.
   * @param ff            The full frame returning from a spawn.
   * @param left_map_ptr  Pointer to our initial left map.
   * @return              The worker that this method returns on. 
   */ 
  static __cilkrts_worker*
    slow_path_reductions_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff,
        struct cilkred_map **left_map_ptr)
    {

      // CILK_ASSERT: w is holding frame lock on parent_ff.
#if REDPAR_DEBUG > 0
      CILK_ASSERT(!ff->rightmost_child);
      CILK_ASSERT(!ff->is_call_child);
#endif

      // Loop invariant:
      // When beginning this loop, we should
      //   1. Be holding the lock on ff->parent.
      //   2. left_map_ptr should be the address of the pointer to the left map.
      //   3. All maps should be slid over left by one, if possible.
      //   4. All exceptions should be merged so far.
      while (1) {

        // Slide middle map left if possible.
        if (!(*left_map_ptr)) {
          *left_map_ptr = w->reducer_map;
          w->reducer_map = NULL;
        }
        // Slide right map to middle if possible.
        if (!w->reducer_map) {
          w->reducer_map = ff->right_reducer_map;
          ff->right_reducer_map = NULL;
        }

        // Since we slid everything left by one,
        // we are finished if there is no middle map.
        if (!w->reducer_map) {
          verify_current_wkr(w);
          return w;
        }
        else {
          struct cilkred_map* left_map;
          struct cilkred_map* middle_map;
          struct cilkred_map* right_map;

          // Take all the maps from their respective locations.
          // We can't leave them in place and execute a reduction because these fields
          // might change once we release the lock.
          left_map = *left_map_ptr;
          *left_map_ptr = NULL;
          middle_map = w->reducer_map;
          w->reducer_map = NULL;
          right_map = ff->right_reducer_map;
          ff->right_reducer_map = NULL;

          // WARNING!!! Lock release here.
          // We have reductions to execute (and we can't hold locks).
          __cilkrts_frame_unlock(w, ff->parent);

          // Merge all reducers into the left map.
          left_map = repeated_merge_reducer_maps(&w,
              left_map,
              middle_map);
          verify_current_wkr(w);
          left_map = repeated_merge_reducer_maps(&w,
              left_map,
              right_map);
          verify_current_wkr(w);
          CILK_ASSERT(NULL == w->reducer_map);
          // Put the final answer back into w->reducer_map.
          w->reducer_map = left_map;

          // Save any exceptions generated because of the reduction
          // process from the returning worker.  These get merged
          // the next time around the loop.
          CILK_ASSERT(NULL == ff->pending_exception);
          ff->pending_exception = w->l->pending_exception;
          w->l->pending_exception = NULL;

          // Lock ff->parent for the next loop around.
          __cilkrts_frame_lock(w, ff->parent);

          // Once we have the lock again, recompute who is to our
          // left.
          splice_left_ptrs left_ptrs;
          left_ptrs = compute_left_ptrs_for_spawn_return(w, ff);

          // Update the pointer for the left map.
          left_map_ptr = left_ptrs.map_ptr;
          // Splice the exceptions for spawn.
          splice_exceptions_for_spawn(w, ff, left_ptrs.exception_ptr);
        }
      }
      // We should never break out of this loop.

      CILK_ASSERT(0);
      return NULL;
    }



  /**
   * Execute reductions when returning from a spawn whose parent has
   * been stolen.
   *
   * Execution may start on w, but may finish on a different worker.
   * This method acquires/releases the lock on ff->parent. 
   *
   * @param w            The currently executing worker.
   * @param ff           The full frame of the spawned function that is returning.
   * @param returning_sf The __cilkrts_stack_frame for this returning function.
   * @return             The worker returning from this method. 
   */ 
  static __cilkrts_worker*
    execute_reductions_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *returning_sf)
    { 
      // Step A1 from reducer protocol described above.
      //
      // Coerce the runtime into thinking that 
      // ff/returning_sf are still on the bottom of
      // w's deque.
      restore_frame_for_spawn_return_reduction(w, ff, returning_sf);

      // Step A2 and A3: Execute reductions on user stack.
      BEGIN_WITH_FRAME_LOCK(w, ff->parent) {
        struct cilkred_map **left_map_ptr;
        left_map_ptr = fast_path_reductions_for_spawn_return(w, ff);

        // Pointer will be non-NULL if there are
        // still reductions to execute.
        if (left_map_ptr) {
          // WARNING: This method call may release the lock
          // on ff->parent and re-acquire it (possibly on a
          // different worker).
          // We can't hold locks while actually executing
          // reduce functions.
          w = slow_path_reductions_for_spawn_return(w,
              ff,
              left_map_ptr);
          verify_current_wkr(w);
        }

        finish_spawn_return_on_user_stack(w, ff->parent, ff);      
        // WARNING: the use of this lock macro is deceptive.
        // The worker may have changed here.
      } END_WITH_FRAME_LOCK(w, ff->parent);
      return w;
    }



  /**
   * Execute fast "reductions" when ff stalls at a sync.
   *
   * @param   w  The currently executing worker.
   * @param   ff The full frame stalling at a sync.
   * @return  1 if we are finished with all reductions after calling this method.
   * @return  0 if we still need to execute the slow path reductions.
   */ 
  static inline
    int fast_path_reductions_for_sync(__cilkrts_worker *w,
        full_frame *ff) {
      // Return 0 if there is some reduction that needs to happen.
      return !(w->reducer_map  || ff->pending_exception);
    }

  /**
   * Executes slow reductions when ff stalls at a sync.
   * This method should execute only if
   *   fast_path_reductions_for_sync(w, ff) returned 0.
   *
   * After this method completes:
   *   1. ff's current reducer map has been deposited into
   *       right_reducer_map of ff's rightmost child, or
   *       ff->children_reducer_map if ff has no children.
   *   2. Similarly for ff's current exception.
   *   3. Nothing to calculate for stacks --- if we are stalling
   *      we will always free a stack.
   *
   * This method may repeatedly acquire/release the lock on ff.
   *
   * @param   w  The currently executing worker.
   * @param   ff The full frame stalling at a sync.
   * @return  The worker returning from this method.
   */
  static __cilkrts_worker*
    slow_path_reductions_for_sync(__cilkrts_worker *w,
        full_frame *ff)
    {
      struct cilkred_map *left_map;
      struct cilkred_map *middle_map;

#if (REDPAR_DEBUG > 0)
      CILK_ASSERT(ff);
      CILK_ASSERT(w->head == w->tail);
#endif

      middle_map = w->reducer_map;
      w->reducer_map = NULL;

      // Loop invariant: middle_map should be valid (the current map to reduce). 
      //                 left_map is junk.
      //                 w->reducer_map == NULL.
      while (1) {
        BEGIN_WITH_FRAME_LOCK(w, ff) {
          splice_left_ptrs left_ptrs = compute_left_ptrs_for_sync(w, ff);

          // Grab the "left" map and store pointers to those locations.
          left_map = *(left_ptrs.map_ptr);
          *(left_ptrs.map_ptr) = NULL;

          // Slide the maps in our struct left as far as possible.
          if (!left_map) {
            left_map = middle_map;
            middle_map = NULL;
          }

          *(left_ptrs.exception_ptr) =
            __cilkrts_merge_pending_exceptions(w,
                *left_ptrs.exception_ptr,
                ff->pending_exception);
          ff->pending_exception = NULL;

          // If there is no middle map, then we are done.
          // Deposit left and return.
          if (!middle_map) {
            *(left_ptrs).map_ptr = left_map;
#if (REDPAR_DEBUG > 0)
            CILK_ASSERT(NULL == w->reducer_map);
#endif
            // Sanity check upon leaving the loop.
            verify_current_wkr(w);
            // Make sure to unlock before we return!
            __cilkrts_frame_unlock(w, ff);
            return w;
          }
        } END_WITH_FRAME_LOCK(w, ff);

        // If we get here, we have a nontrivial reduction to execute.
        middle_map = repeated_merge_reducer_maps(&w,
            left_map,
            middle_map);
        verify_current_wkr(w);

        // Save any exceptions generated because of the reduction
        // process.  These get merged the next time around the
        // loop.
        CILK_ASSERT(NULL == ff->pending_exception);
        ff->pending_exception = w->l->pending_exception;
        w->l->pending_exception = NULL;
      }

      // We should never break out of the loop above.
      CILK_ASSERT(0);
      return NULL;
    }


  /**
   * Execute reductions when ff stalls at a sync.
   *
   * Execution starts on w, but may finish on a different worker.
   * This method may acquire/release the lock on ff.
   *
   * @param w          The currently executing worker.
   * @param ff         The full frame of the spawned function at the sync
   * @param sf_at_sync The __cilkrts_stack_frame stalling at a sync
   * @return           The worker returning from this method.
   */ 
  static __cilkrts_worker*
    execute_reductions_for_sync(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *sf_at_sync)
    {
      int finished_reductions;
      // Step B1 from reducer protocol above:
      // Restore runtime invariants.
      //
      // The following code for this step is almost equivalent to
      // the following sequence:
      //   1. disown(w, ff, sf_at_sync, "sync") (which itself
      //        calls make_unrunnable(w, ff, sf_at_sync))
      //   2. make_runnable(w, ff, sf_at_sync).
      //
      // The "disown" will mark the frame "sf_at_sync"
      // as stolen and suspended, and save its place on the stack,
      // so it can be resumed after the sync. 
      //
      // The difference is, that we don't want the disown to 
      // break the following connections yet, since we are
      // about to immediately make sf/ff runnable again anyway.
      //   sf_at_sync->worker == w
      //   w->l->frame_ff == ff.
      //
      // These connections are needed for parallel reductions, since
      // we will use sf / ff as the stack frame / full frame for
      // executing any potential reductions.
      //
      // TBD: Can we refactor the disown / make_unrunnable code
      // to avoid the code duplication here?

      ff->call_stack = NULL;

      // Normally, "make_unrunnable" would add CILK_FRAME_STOLEN and
      // CILK_FRAME_SUSPENDED to sf_at_sync->flags and save the state of
      // the stack so that a worker can resume the frame in the correct
      // place.
      //
      // But on this path, CILK_FRAME_STOLEN should already be set.
      // Also, we technically don't want to suspend the frame until
      // the reduction finishes.
      // We do, however, need to save the stack before
      // we start any reductions, since the reductions might push more
      // data onto the stack.
      CILK_ASSERT(sf_at_sync->flags | CILK_FRAME_STOLEN);
      __cilkrts_put_stack(ff, sf_at_sync);
      __cilkrts_make_unrunnable_sysdep(w, ff, sf_at_sync, 1,
          "execute_reductions_for_sync");
      CILK_ASSERT(w->l->frame_ff == ff);

      // Step B2: Execute reductions on user stack.
      // Check if we have any "real" reductions to do.
      finished_reductions = fast_path_reductions_for_sync(w, ff);

      if (!finished_reductions) {
        // Still have some real reductions to execute.
        // Run them here.

        // This method may acquire/release the lock on ff.
        w = slow_path_reductions_for_sync(w, ff);

        // The previous call may return on a different worker.
        // than what we started on.
        verify_current_wkr(w);
      }

#if REDPAR_DEBUG >= 0
      CILK_ASSERT(w->l->frame_ff == ff);
      CILK_ASSERT(ff->call_stack == NULL);
#endif

      // Now we suspend the frame ff (since we've
      // finished the reductions).  Roughly, we've split apart the 
      // "make_unrunnable" call here --- we've already saved the
      // stack info earlier before the reductions execute.
      // All that remains is to restore the call stack back into the
      // full frame, and mark the frame as suspended.
      ff->call_stack = sf_at_sync;
      sf_at_sync->flags |= CILK_FRAME_SUSPENDED;

      return w;
    }


  /*
     Local Variables: **
     c-file-style:"bsd" **
     c-basic-offset:4 **
     indent-tabs-mode:nil **
End: **
*/
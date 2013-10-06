/* scheduler.h                  -*-C++-*-
 *
 *************************************************************************
 *
 * Copyright (C) 2009-2011 
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
 **************************************************************************/

/**
 * @file scheduler.h
 *
 * @brief scheduler.h declares routines for the Intel Cilk Plus scheduler,
 * making it the heart of the Intel Cilk Plus implementation.
 */

#ifndef INCLUDED_SCHEDULER_DOT_H
#define INCLUDED_SCHEDULER_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>

#include <core/rts-common.h>
#include <core/full_frame.h>
#include <core/global_state.h>
#include <core/meta_schedulers.h>
#include <reducers/reducer_impl.h>

__CILKRTS_BEGIN_EXTERN_C

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

//CSZ: not needed. legacy 
//#ifndef _WIN32
    // TBD: definition of max() for Linux.
//#   define max(a, b) ((a) < (b) ? (b) : (a))
//#endif

// Set to 0 to allow parallel reductions.
#define DISABLE_PARALLEL_REDUCERS 0
#define REDPAR_DEBUG 0

//TODO: move this to be environment controlled dynamically.
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


#ifdef CILK_IVARS
#include "local_state.h"
NORETURN longjmp_into_runtime(__cilkrts_worker *w, scheduling_stack_fcn_t fcn, __cilkrts_stack_frame *sf);
void self_steal(__cilkrts_worker *w);
void self_steal_return(__cilkrts_worker *w);
void make_unrunnable(__cilkrts_worker *w,
                            full_frame *ff,
                            __cilkrts_stack_frame *sf,
                            int state_valid,
                            const char *why);
#endif

/**
 * Lock the worker mutex to allow exclusive access to the values in the
 * __cilkrts_worker and local_state structures.
 *
 * Preconditions:
 * - local_state.don_not_steal must not be set.  Essentially this asserts
 * that the worker is not locked recursively.
 *
 * @param w The worker to lock.
 */
COMMON_PORTABLE
void __cilkrts_worker_lock(__cilkrts_worker *w);

/**
 * Unlock the worker mutex.
 *
 * Preconditions:
 * - local_state.don_not_steal must be set.  Essentially this asserts
 * that the worker has been previously locked.
 *
 * @param w The worker to unlock.
 */
COMMON_PORTABLE
void __cilkrts_worker_unlock(__cilkrts_worker *w);

/**
 * Push the next full frame to be made active in this worker and increment
 * its join counter.
 *
 * __cilkrts_push_next_frame and pop_next_frame work on a one-element queue.
 * This queue is used to communicate across the runtime from the code that
 * wants to activate a frame to the code that can actually begin execution
 * on that frame.  They are asymetrical in that push increments the join
 * counter but pop does not decrement it.  Rather, a single push/pop
 * combination makes a frame active and increments its join counter once.
 *
 * Note that a system worker may chose to push work onto a user worker if
 * the work is the continuation from a sync which only the user worker may
 * complete.
 *
 * @param w The worker which the frame is to be pushed onto.
 * @param ff The full_frame which is to be continued by the worker.
 */
COMMON_PORTABLE
void __cilkrts_push_next_frame(__cilkrts_worker *w,
                               full_frame *ff);

/**
 * Sync on this worker. If this is the last worker to reach the sync,
 * execution may resume on this worker after the sync. If this is not
 * the last spawned child to reach the sync, then execution is suspended
 * and the worker will re-enter the scheduling loop, looking for work
 * it can steal.
 *
 * This function will jump into the runtime to switch to the scheduling
 * stack to implement most of its logic.
 *
 * @param w The worker which is executing the sync.
 * @param sf The __cilkrts_stack_frame containing the sync.
 */
COMMON_PORTABLE
NORETURN __cilkrts_c_sync(__cilkrts_worker *w,
                          __cilkrts_stack_frame *sf);

/**
 * Worker W completely promotes its own deque, simulating the case
 * where the whole deque is stolen.  We use this mechanism to force
 * the allocation of new storage for reducers for race-detection
 * purposes.
 *
 * This is called from the reducer lookup logic when g->force_reduce
 * is set.
 *
 * @warning Use of "force_reduce" is known to have bugs when run with
 * more than 1 worker.
 *
 * @param w The worker which is to have all entries in its deque
 * promoted to full frames.
 */
COMMON_PORTABLE
void __cilkrts_promote_own_deque(__cilkrts_worker *w);

/**
 * Called when a function attempts to return from a spawn and the
 * parent has been stolen.  While this function can return, it
 * will most likely jump into the runtime to switch onto the
 * scheduling stack to execute do_return_from_spawn().
 *
 * @param w The worker which attempting to return from a spawn to
 * a stolen parent.
 * @param returning_sf The stack frame which is returning. 
 */
COMMON_PORTABLE
void __cilkrts_c_THE_exception_check(__cilkrts_worker *w,
				     __cilkrts_stack_frame *returning_sf);

/**
 * Used by the gcc implementation of exceptions to return an exception
 * to a stolen parent
 *
 * @param w The worker which attempting to return from a spawn with an
 * exception to a stolen parent.
 */
COMMON_PORTABLE
NORETURN __cilkrts_exception_from_spawn(__cilkrts_worker *w,
					__cilkrts_stack_frame *returning_sf);

/**
 * Used by the Windows implementations of exceptions to migrate an exception
 * across fibers.  Call this function when an exception has been thrown and 
 * has to traverse across a steal.  The exception has already been wrapped up,
 * so all that remains is to longjmp() into the continuation, sync, and
 * re-raise it.
 *
 * @param sf The __cilkrts_stack_frame for the frame that is attempting to
 * return an exception to a stolen parent.
 */
void __cilkrts_migrate_exception (__cilkrts_stack_frame *sf);

/**
 * Return from a call, not a spawn, where this frame has ever been stolen.
 *
 * @param w The worker that is returning from a frame which was ever stolen.
 */
COMMON_PORTABLE
void __cilkrts_return(__cilkrts_worker *w);

/**
 * Special return from the initial frame.  Will be called from
 * __cilkrts_leave_frame if CILK_FRAME_LAST is set.
 *
 * This function will do the things necessary to cleanup, and unbind the
 * thread from the Intel Cilk Plus runtime.  If this is the last user
 * worker unbinding from the runtime, all system worker threads will be
 * suspended.
 *
 * Preconditions:
 * - This must be a user worker.
 *
 * @param w The worker that's returning from the initial frame.
 */
COMMON_PORTABLE
void __cilkrts_c_return_from_initial(__cilkrts_worker *w);

/**
 * Used by exception handling code to pop an entry from the worker's deque.
 *
 * @param w Worker to pop the entry from
 *
 * @return __cilkrts_stack_frame of parent call
 * @return NULL if the deque is empty
 */
COMMON_PORTABLE
__cilkrts_stack_frame *__cilkrts_pop_tail(__cilkrts_worker *w);

/**
 * Modifies the worker's protected_tail to prevent frames from being stolen.
 *
 * The Dekker protocol has been extended to only steal if head+1 is also
 * less than protected_tail.
 *
 * @param w The worker to be modified.
 * @param new_protected_tail The new setting for protected_tail, or NULL if the
 * entire deque is to be protected
 *
 * @return Previous value of protected tail.
 */
COMMON_PORTABLE
__cilkrts_stack_frame *volatile *__cilkrts_disallow_stealing(
    __cilkrts_worker *w,
    __cilkrts_stack_frame *volatile *new_protected_tail);

/**
 * Restores the protected tail to a previous state, possibly allowing frames
 * to be stolen.
 *
 * @param w The worker to be modified.
 * @param saved_protected_tail A previous setting for protected_tail that is
 * to be restored
 */
COMMON_PORTABLE
void __cilkrts_restore_stealing(
    __cilkrts_worker *w,
    __cilkrts_stack_frame *volatile *saved_protected_tail);

/**
 * Initialize a __cilkrts_worker.  The memory for the worker must have been
 * allocated outside this call.
 *
 * @param g The global_state_t.
 * @param self The index into the global_state's array of workers for this
 * worker, or -1 if this worker was allocated from the heap and cannot be
 * stolen from.
 * @param w The worker to be initialized.
 *
 * @return The initialized __cilkrts_worker.
 */
COMMON_PORTABLE
__cilkrts_worker *make_worker(global_state_t *g,
                              int self,
                              __cilkrts_worker *w);

/**
 * Free up any resources allocated for a worker.  The memory for the
 * __cilkrts_worker itself must be deallocated outside this call.
 *
 * @param w The worker to be destroyed.
 */
COMMON_PORTABLE
void destroy_worker (__cilkrts_worker *w);

/**
 * Initialize the runtime.  If necessary, allocates and initializes the
 * global state.  If necessary, unsuspends the system workers.
 *
 * @param start Specifies whether the workers are to be unsuspended if
 * they are suspended.  Allows __cilkrts_init() to start up the runtime without
 * releasing the system threads.
 */
COMMON_PORTABLE
void __cilkrts_init_internal(int start);

/**
 * Part of the sequence to shutdown the runtime.  Specifically frees the
 * global_state_t for the runtime.
 *
 * @param g The global_state_t.
 */
COMMON_PORTABLE
void __cilkrts_deinit_internal(global_state_t *g);

/**
 * Obsolete.  We no longer need to import or export reducer maps.
 */
COMMON_PORTABLE
cilkred_map *__cilkrts_xchg_reducer(
    __cilkrts_worker *w, cilkred_map *newmap) cilk_nothrow;

/**
 * Called when a user thread is bound to the runtime.  If this increments the
 * count of bound user threads from 0 to 1, the system worker threads are
 * unsuspended.
 *
 * @param g The runtime global state.
 *
 * Preconditions:
 * - Global lock must be held.
 */
COMMON_PORTABLE
void __cilkrts_enter_cilk(global_state_t *g);

/**
 * Called when a user thread is unbound from the runtime.  If this decrements
 * the count of bound user threads to 0, the system worker threads are
 * suspended.
 *
 * @param g The runtime global state.
 *
 * Preconditions:
 * - Global lock must be held.
 */
COMMON_PORTABLE
void __cilkrts_leave_cilk(global_state_t *g);


/**
 * Prints out Cilk runtime statistics.
 *
 * @param g The runtime global state.
 *
 * This method is useful only for debugging purposes.  No guarantees
 * are made as to the validity of this data. :)
 */
COMMON_PORTABLE
void __cilkrts_dump_stats_to_stderr(global_state_t *g);

#ifdef CILK_IVARS
full_frame *pop_next_frame(__cilkrts_worker *w);
void push_child(full_frame *parent_ff, full_frame *child_ff);
void make_runnable(__cilkrts_worker *w, full_frame *ff);
void make_unrunnable(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf, int state_valid, const char *why);
void do_work(__cilkrts_worker *w, full_frame *ff);
void __cilkrts_push_next_frame(__cilkrts_worker *w, full_frame *ff);
int can_steal_from(__cilkrts_worker *victim);

/* Return true if the frame can be stolen, false otherwise */
int dekker_protocol(__cilkrts_worker *victim);
__cilkrts_stack_frame *__cilkrts_advance_frame(__cilkrts_stack_frame *sf);
#endif

//TODO: remove all these from here and into their own reduction module

  /*************************************************************
    Forward declarations for reduction protocol.
   *************************************************************/

   __cilkrts_worker*
    execute_reductions_for_sync(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *sf_at_sync);

   __cilkrts_worker*
    execute_reductions_for_spawn_return(__cilkrts_worker *w,
        full_frame *ff,
        __cilkrts_stack_frame *returning_sf);

int provably_good_steal(__cilkrts_worker *w, full_frame *ff);
int fast_path_reductions_for_sync(__cilkrts_worker *w, full_frame *ff);
__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_SCHEDULER_DOT_H)

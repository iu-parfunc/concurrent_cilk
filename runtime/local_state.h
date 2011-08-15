/* local_state.h                  -*-C++-*-
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
 * @file local_state.h
 *
 * @brief The local_state structure contains additional OS-independent
 * information that's associated with a worker, but doesn't need to be visible
 * to the code generated by the compiler.
 */

#ifndef INCLUDED_LOCAL_STATE_DOT_H
#define INCLUDED_LOCAL_STATE_DOT_H

#include <internal/abi.h>
#include "worker_mutex.h"
#include "global_state.h"

#include <setjmp.h>
#include <stddef.h>

#ifndef _WIN32
#   include <pthread.h>
#endif

__CILKRTS_BEGIN_EXTERN_C

/* Opaque types. */
typedef struct signal_node_t signal_node_t;
struct full_frame;
struct free_list;
struct pending_exception_info;

/**
 * Magic numbers for local_state, used for debugging
 */
typedef unsigned long long ls_magic_t;

/**
 * Scheduling stack function: A function that is decided on the program stack,
 * but that must be executed on the scheduling stack.
 */
typedef void (*scheduling_stack_fcn_t) (__cilkrts_worker *w,
                                        struct full_frame *f,
                                        __cilkrts_stack_frame *sf);

/**
 * Type of this worker.
 **/
typedef enum cilk_worker_type
{
    WORKER_FREE,    ///< Unused worker - available to be bound to user threads
    WORKER_SYSTEM,  ///< Worker created by runtime - able to steal from any worker
    WORKER_USER     ///< User thread - able to steal only from team members
} cilk_worker_type;

/**
 * The local_state structure contains additional OS-independent information
 * that's associated with a worker, but doesn't need to be visible to the
 * compiler.  No compiler-generated code should need to know the layout of
 * this structure.
 */
/* COMMON_PORTABLE */
typedef struct local_state
{
    /** This value should be in the first field in any local_state */
#   define WORKER_MAGIC_0 ((ls_magic_t)0xe0831a4a940c60b8ULL)

    /** Should be WORKER_MAGIC_0 or the local_state has been corrupted */
    ls_magic_t worker_magic_0;

    /** Mutex used to serialize access to the local_state */
    struct mutex lock;

    /**
     * Flag that indicates that the worker is interested in grabbing
     * LOCK, and thus thieves should leave the worker alone.
     */
    int do_not_steal;

    /**
     * Lock that all thieves grab in order to compete for the right
     * to disturb this worker.
     */
    struct mutex steal_lock;

    /** Full frame that the worker is working on */
    struct full_frame *frame;

    /** Full frame that the worker will be working on next */
    struct full_frame *next_frame;

    /** Lazy task queue of this worker - an array of pointers to stack frames */
    __cilkrts_stack_frame **ltq;

    /** Stacks waiting to be reused */
    __cilkrts_stack_cache stack_cache;

    /** Net stacks allocated - freed.  May be negative. */
    long net_stacks_allocated;

    /** Stacks allocated.  Nonnegative. */
    long stacks_allocated;

    /** State fof the random number generator */
    unsigned rand_seed;

    /** Type of this worker */
    cilk_worker_type type;

    /**
     * jmp_buf used to jump back into the runtime system after an
     * unsuccessful steal check or sync.
     */
    jmp_buf env;

    /** Function to execute after transferring onto the scheduling stack. */
    scheduling_stack_fcn_t post_suspend;

    /**
     * __cilkrts_stack_frame we suspended when we transferred onto the
     * scheduling stack.
     */
    __cilkrts_stack_frame *suspended_stack;

    /**
     * Saved exception object for an exception that is being passed to
     * our parent
     */
    struct pending_exception_info *pending_exception;

    /// Place to save return address so we can report it to Inspector
    void *sync_return_address;

    /** Buckets for the memory allocator */
    struct free_list *free_list[FRAME_MALLOC_NBUCKETS];

    /** Potential function for the memory allocator */
    size_t bucket_potential[FRAME_MALLOC_NBUCKETS];

    /** Support for statistics */
    statistics stats;

    /**
     * Count indicates number of failures since last successful steal.  This is
     * used by the scheduler to reduce contention on shared flags.
     */
    unsigned int steal_failure_count;

    /**
     * Team on which this worker is a participant.  When a user worker enters,
     * its team is its own worker struct and it can never change teams.  When a
     * system worker steals, it adopts the team of its victim.
     */
    __cilkrts_worker *team;

    /**
     * This is set iff this is a WORKER_USER and there has been a steal.  It
     * points to the first frame that was stolen since the team was last fully
     * sync'd.  Only this worker may continue past a sync in this function.
     */
    struct full_frame *last_full_frame;

    /**
     * NULL for WORKER_SYSTEMs (they are created on their scheduling stacks, so
     * they already know where their scheduling stacks are).  A WORKER_USER can
     * jump to this stack when it returns to a stolen parent and wants to begin
     * stealing.
     */
    void *scheduler_stack;

    /**
     * 0 if the user thread has not yet been imported.  1 if the user thread
     * has been imported.  \"Imported\" means the user thread has returned to a
     * stolen parent and a scheduling stack or fiber has been created for it.
     * Ignored for system workers.
     */
    int user_thread_imported;

    /**
     * 1 if work was stolen from another worker.  When true, this will flag
     * setup_for_execution_pedigree to increment the pedigree when we resume
     * execution to match the increment that would have been done on a return
     * from a spawn helper.
     */
    int work_stolen;

    /**
     * Separate the signal_node from other things in the local_state by the
     * sizeof a cache line for performance reasons.
     */
    char buf[64];

    /**
     * Signal object for waking/sleeping the worker.  This should be a pointer
     * to avoid the possibility of caching problems.
     */
     signal_node_t *signal_node;

    /** This value should be in the last field in any local_state */
#   define WORKER_MAGIC_1 ((ls_magic_t)0x16164afb0ea0dff9ULL)

    /** Should be WORKER_MAGIC_1 or the local_state has been corrupted */
    ls_magic_t worker_magic_1;
} local_state;

/**
 * Perform cleanup according to the function set before the longjmp().
 *
 * Call this after longjmp() has completed and the worker is back on a
 * scheduling stack.
 *
 * @param w __cilkrts_worker currently executing.
 */
void run_scheduling_stack_fcn(__cilkrts_worker *w);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_LOCAL_STATE_DOT_H)

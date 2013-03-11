/*
 *
 * Copyright (C) 2009-2011 , Intel Corporation
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *   * Neither the name of Intel Corporation nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
// NOTE: Presently this code is CONDITIONALLY included/compiled into another file.

// Thus this whole file is implicitly IFDEF CILK_IVARS

// ====================================================================================================

// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.

// API & Usage notes:
// 
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.
//
//  Paused stacks are represented by pointers.  Presently they may only be woken once.
//  (Waking frees the heap object containing the paused stack.)


// For my_resume only (SP): 
#include "concurrent_cilk_internal.h"
#include "jmpbuf.h"
#include <pthread.h>
#include <stdio.h>
#include "local_state.h"
#include "scheduler.h"
#include <time.h>

#include "concurrent_cilk_forwarding_array.h"
#include "concurrent_cilk_forwarding_array.c"


// ================================================================================
// TEMP: DUPLICATED MACRO DEFINITIONS:
#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)

extern void __cilkrts_concurrent_yield(__cilkrts_worker *w);
int can_steal_from(__cilkrts_worker *victim);
void pthread_yield();

void __cilkrts_concurrent_yield(struct __cilkrts_worker *w)
{
#if __APPLE__ || __MIC__
  sched_yield();
#else
  pthread_yield();
#endif
}


CILK_API(void) __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

CILK_API(void) __cilkrts_usleep(unsigned long micros)
{
  struct timespec time;
  time.tv_sec = micros/1000000ul;
  time.tv_nsec = (micros % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

inline
CILK_API(void) __cilkrts_ivar_clear(__cilkrts_ivar* ivar)
{
  *ivar = 0;
}

inline static void restore_paused_worker(__cilkrts_worker *old_w) 
{
  __cilkrts_paused_stack *stk = old_w->pstk;
  //the worker to restore
  __cilkrts_worker *w = (__cilkrts_worker *) stk->orig_worker;

  //if the worker came into the scheduler
  //there should be no more work left to de
  //since we don't preempt our threads
  CILK_ASSERT(!can_steal_from(old_w));
  CILK_ASSERT(!can_steal_from((__cilkrts_worker *) stk->replacement_worker));

  //remove the paused stack that we are restoring so that it doesn't get run again. 
  old_w->pstk = NULL;

  //remove the old worker form the forwarding array 
  remove_replacement_worker(old_w);

  //cache the paused stack for reuse
  enqueue(w->paused_stack_cache, (ELEMENT_TYPE) stk);

  //restore the original blocked worker
  //at this point. the scheduler forgets about tlsw
  __cilkrts_set_tls_worker(w);

  // only cache replacement workers.
  if(old_w->self < 0) 
    enqueue(w->worker_cache, (ELEMENT_TYPE) old_w); 

  //--------------------------- restore the context -------------------
  CILK_LONGJMP(w->current_stack_frame->ctx);

  //does not return
  CILK_ASSERT(0);
}

// This initiializes the data structure that represents a paused stack, but save_worker
// must be called subsequently to fully populate it.
inline
__cilkrts_paused_stack* make_paused_stack(__cilkrts_worker* w)
{
  __cilkrts_paused_stack* sustk = NULL; 

  dequeue(w->paused_stack_cache, (ELEMENT_TYPE *) &sustk);
  if_f(!sustk)
    sustk = (__cilkrts_paused_stack *) memalign(64, sizeof(__cilkrts_paused_stack));

  //system workers might not have a frame!
  if(w->l->frame_ff) 
    sustk->stack = w->l->frame_ff->stack_self; 
  else
    sustk->stack = NULL;

  sustk->orig_worker        = w;
  sustk->replacement_worker = NULL;
  return sustk;
}

// Commit the pause.
inline
  CILK_API(void)
__cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack* stk) 
{
  // create a new replacement worker:
  __cilkrts_worker* new_w = get_replacement_worker(w, stk);

  //register the replacement worker for stealing and
  //additionally have the replacement inherit the parent's
  //forwarding array.
  add_replacement_worker(w, new_w, stk);

  // head to the scheduler with the replacement worker
  __cilkrts_run_scheduler_with_exceptions(new_w); // calls __cilkrts_scheduler
  CILK_ASSERT(0); //no return
}

// Back out of the pause before making any externally visible changes.
// The client better not have stored the __cilkrts_paused_stack anywhere!
inline
CILK_API(void) __cilkrts_undo_pause(__cilkrts_worker *w, __cilkrts_paused_stack* stk) 
{
  //cache the paused stack for reuse
  enqueue(w->paused_stack_cache, (ELEMENT_TYPE) stk);
}

// This function is a `yield` for defining coroutines.  It stops the current computation but adds it
// to a ready queue to be awoken after other tasks are given a chance to run.
//DEPRECATED/does not work. the whole api has change. concurrent_yield and concurrent_wake should 
//be implemented!
CILK_API(void) __cilkrts_pause_a_bit(struct __cilkrts_worker* w)
{
  // Here we do not need to save anything... the worker is left in place, but the thread abandons it.
  // Save the continuation in the top stack frame of the old (stalled) worker:
  struct __cilkrts_paused_stack* ptr = __cilkrts_pause(w);
  if(ptr) {
    __cilkrts_wake_stack(ptr);
    __cilkrts_finalize_pause(w,ptr);
  }
}

// Mark a paused stack as ready by populating the workers pstk pointer.
// multiple writes are idempotent
inline
CILK_API(void) __cilkrts_wake_stack(__cilkrts_paused_stack* stk)
{
  stk->replacement_worker->pstk = stk;
}

// Helper used for debugging:
void __cilkrts_show_threadid()
{
  pthread_t id = pthread_self();
  fprintf(stderr, "TID %lu ", (unsigned long)id);
}

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


// ================================================================================
// TEMP: DUPLICATED MACRO DEFINITIONS:
#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)

extern void __cilkrts_concurrent_yield(__cilkrts_worker *w);

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

CILK_API(void) __cilkrts_ivar_clear(__cilkrts_ivar* ivar) {
  ivar->__header = 0; // Empty ivar.
}

/* CILK FUNCTIONS CALLED:
 * NONE
 *
 returns 1 on successful lock, and 0 on failure */
int paused_stack_lock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk) {

  IVAR_DBG_PRINT_(1,"[concurrent-cilk, paused_stack_lock] locking stack %p with worker owner: %d/%p\n", stk, w->self,w);
  // two threads race for lock inform, and one comes out on top. 
  // this thread then grabs the lock_success value and holds the lock
  if(__sync_bool_compare_and_swap(&stk->lock_inform, 0, 1))
    if(__sync_bool_compare_and_swap(&stk->lock_success, 0, 1))
      return 1;
  return 0;
}
/* CILK FUNCTIONS CALLED:
 * NONE
 *
 returns 1 on successfull unlock, and 0 on failure */
int paused_stack_unlock(__cilkrts_worker *w, volatile __cilkrts_paused_stack* stk)
{
  IVAR_DBG_PRINT_(1,"[concurrent-cilk, paused_stack_unlock] unlocking stack %p with worker owner: %d/%p\n", stk, w->self,w);
  // two threads race for lock inform, and one comes out on top. 
  // this thread then grabs the lock_success value and holds the lock
  if(__sync_bool_compare_and_swap(&stk->lock_success, 1, 0))
    if(__sync_bool_compare_and_swap(&stk->lock_inform, 1, 0))
      return 1;
  return 0;
}

/* CILK FUCTIONS CALLED
 *  CILK_ASSERT
 *  __cilkrts_cilkscreen_ignore_block
 *  make_worker
 *  make_worker_system
 *  sysdep_make_tiny_stack
 *  signal_node_message
 *  __cilkrts_fence
 *  __cilkrts_set_tls_worker
 Create a new worker. */  
  NOINLINE
struct __cilkrts_worker* replace_worker (__cilkrts_worker* old_w, __cilkrts_worker* fresh_worker, volatile __cilkrts_paused_stack* stk) 
{   
  CILK_ASSERT(old_w);
  CILK_ASSERT(fresh_worker);
  CILK_ASSERT(stk);

  // WARNING: FUTURE CILKSCREEN SUPPORT WOULD REQUIRE MORE WORK HERE:
  // __cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker + sizeof(struct __cilkrts_worker));

  // Here we create a COMPLETELY NEW WORKER. 
  __cilkrts_cilkscreen_ignore_block(fresh_worker, fresh_worker+1);
  fresh_worker = (__cilkrts_worker *) make_worker(old_w->g, -1, fresh_worker); // Using self = -1 for now.

  // Initialize the new worker:
  //---------------------------------   

  // Should the replacement be a user worker or a system worker?
  // It should be a system worker.
  //  #define CONCCILK_WORKER_USER
#ifdef CONCCILK_WORKER_USER
  fresh_worker->l->type = WORKER_USER;
  fresh_worker->l->team = fresh_worker;
#else
  fresh_worker->l->type = WORKER_FREE;
  make_worker_system(fresh_worker);
  signal_node_msg(fresh_worker->l->signal_node, 1); // Set 'run' field to 1

#endif

  fresh_worker->l->scheduler_stack = sysdep_make_tiny_stack(fresh_worker);
  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker] Created scheduler stack for replacement: %p\n", fresh_worker->l->scheduler_stack);
  __cilkrts_fence(); //probably redundant

  //---------------------------------
  // Label the fresh worker as a replacement, and link the old worker to it:
  fresh_worker->is_replacement = 1;
  stk->replacement_worker = fresh_worker;
  __cilkrts_fence();
  CILK_ASSERT(stk->replacement_worker);

  fresh_worker->blocked_parent = old_w;
  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker] Created REPLACEMENT worker %d/%p, paused stack %p\n", 
      fresh_worker->self, fresh_worker, stk);
  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker] old worker %d/%p\n", 
      old_w->self, old_w);

  BEGIN_WITH_WORKER_LOCK(old_w) {

    if (old_w->forwarding_pointer) 
      IVAR_DBG_PRINT_(2," [UNIMPLEMENTED] worker is replaced TWICE, need to keep the old target of the forwarding pointer available for work stealing\n");

    old_w->forwarding_pointer = fresh_worker;
#ifdef CILK_IVARS_CACHING
    fresh_worker->reference_count++; //increment the reference count
#endif

  } END_WITH_WORKER_LOCK(old_w);

  // At this point the current OS thread should forget about the old worker:
  __cilkrts_set_tls_worker(fresh_worker);

  IVAR_DBG_PRINT_(1," [concurrent-cilk, replace_worker]   Done creating & setting up forwarding pointer to %p (paused stack %p)\n", 
      fresh_worker, stk);                    
  return fresh_worker;
}

/* CILK FUNCTIONS CALLED
 * CILK_LONGJMP
 * CILK_ASSERT
 * SP

 * concurrent

 Resume work that was paused but is now ready to proceed. */
  NOINLINE
void my_resume (__cilkrts_worker *w, full_frame *f, __cilkrts_stack_frame *sf)
{
  IVAR_DBG_PRINT_(2,"[concurrent cilk, my_resume] in my_resume with worker %p and stack %p\n\n",w ,sf);
  void *sp = SP(sf);
  /* Debugging: make sure stack is accessible. */
  ((volatile char *)sp)[-1];

  CILK_LONGJMP(sf->ctx);
  CILK_ASSERT(0); //should never get here
}

/* CILK FUNCTION CALLED
 * __cilkrts_get_tls_worker_fast 
 * CILK_ASSERT
 * __cilkrts_short_pause ----- do we need this still?
 * paused_stack_unlock 
 * __cilkrts_set_tls_worker
 * my_resume

COMMENTS:
right now we are not reaping our workers. We might in the future wish to do so.
However, if we do decide to do this, we will probably run into problems patching up replacements

Takes a pointer to the no-longer-needed replacement worker and the paused worker to resume.*/
void restore_worker2(__cilkrts_worker* old_w, volatile __cilkrts_paused_stack* stk)
{
  //current worker
  __cilkrts_worker* tlsw = __cilkrts_get_tls_worker_fast();
  //the worker to restore
  __cilkrts_worker* w = (__cilkrts_worker *) stk->orig_worker;

  // no replacement or no orig_worker means something is seriously wrong 
  CILK_ASSERT(w);
  CILK_ASSERT(stk->replacement_worker);

  //if we are at the end of the chain of replacement workers,
  //we can dereference its forwarding pointer
  if (stk->replacement_worker->forwarding_pointer == NULL) {
#ifdef CILK_IVARS_CACHING
    if(w->forwarding_pointer !=NULL) w->forwarding_pointer->reference_count--;
#endif
    w->forwarding_pointer = NULL;
  }

  //release a lock on the stack
  while(!paused_stack_unlock(old_w, stk))
    __cilkrts_short_pause();

#ifdef CILK_IVARS_CACHING
  enqueue_paused_stack(w->paused_stack_cache, (void *) stk);
#else
  __cilkrts_free(stk);
#endif
  IVAR_DBG_PRINT_(1," [concurrent-cilk, restore_worker2] Unthawing stalled worker %d with paused stack %p.\n", w->self, stk);


  // restore the original blocked worker
  // at this point. the scheduler forgets about tlsw
  __cilkrts_set_tls_worker(w);

  // reduced the number of paused stacks in existence
  __sync_fetch_and_sub(&w->g->num_paused_stacks,1);

#ifdef CILK_IVARS_CACHING
  // only cache replacement workers.
  if(old_w->self < 0) {
    IVAR_DBG_PRINT_(1," [concurrent-cilk, restore_worker2] cached worker %d/%p\n", old_w->self, old_w);
    old_w->cached = 1;
    __cilkrts_worker_lock(old_w); //lock the worker. no one should be able to reference us while we are waiting
    enqueue_paused_stack(w->worker_cache, old_w); 
  }
#else
  IVAR_DBG_PRINT_(1, "[concurrent-cilk] WARNING: leaking a replacement worker. the RTS does not give workers back to the OS!  please define CILK_IVARS_CACHING to reuse old replacement workers\n");
#endif

  my_resume(w, w->l->frame_ff, w->current_stack_frame);

  // Dont come back
  CILK_ASSERT(0);
}

// ================================================================================

// This initiializes the data structure that represents a paused stack, but save_worker
// must be called subsequently to fully populate it.
__cilkrts_paused_stack* make_paused_stack(__cilkrts_worker* w)
{
  CILK_ASSERT(w);
  __cilkrts_paused_stack* sustk; 

#ifdef CILK_IVARS_CACHING
  if((sustk = (__cilkrts_paused_stack *) dequeue_paused_stack(w->paused_stack_cache)) == NULL)
    sustk = (__cilkrts_paused_stack *)__cilkrts_malloc(sizeof(__cilkrts_paused_stack));
  else
    IVAR_DBG_PRINT_(1,"[concurrent-cilk] got new CACHED stack %p \n", sustk);
#else
  sustk = (__cilkrts_paused_stack *)__cilkrts_malloc(sizeof(__cilkrts_paused_stack));
#endif

  //system workers might not have a frame!
  if(w->l->frame_ff)
    sustk->stack = w->l->frame_ff->stack_self; 
  else
    sustk->stack = NULL;

  sustk->orig_worker        = w;
  sustk->replacement_worker = NULL;

  sustk->ready        = 0;
  sustk->lock_inform  = 0;
  sustk->lock_success = 0;
  return sustk;
}

// DUPLICATE CODE: from __cilkrts_sysdep_import_user_thread and worker_user_scheduler:
NORETURN setup_and_invoke_scheduler(__cilkrts_worker *w) 
{
  __cilkrts_worker_lock(w);
  void *ctx[5]; // Jump buffer for __builtin_setjmp/longjmp.

  CILK_ASSERT(w->l->scheduler_stack);
  if (0 == __builtin_setjmp(ctx)) {
    ctx[2] = w->l->scheduler_stack; // replace the stack pointer.
    __builtin_longjmp(ctx, 1);
  } else {

    IVAR_DBG_PRINT_(1," [concurrent-cilk, setup and invoke] BEGIN SCHEDULING on new thread\n");
    CILK_ASSERT( ! w->l->post_suspend );
    w->reducer_map = 0; // like worker_user_scheduler and do_work..
    __cilkrts_worker_unlock(w);

    __cilkrts_run_scheduler_with_exceptions(w); // calls __cilkrts_scheduler
    CILK_ASSERT(0); //no return
  }
}

// Commit the pause.
//NORETURN 
  CILK_API(void)
__cilkrts_finalize_pause(__cilkrts_worker* w, volatile __cilkrts_paused_stack* stk) 
{
  IVAR_DBG_PRINT_(2," [concurrent-cilk,finalize pause]  finalize_pause: blocked worker %d/%p created a paused stack %p.\n", 
      w->self, w, stk);

  CILK_ASSERT(stk);
  CILK_ASSERT(w);

  // obtain a lock on the stack
  while(!paused_stack_lock(w,stk))
    __cilkrts_short_pause();

  // create a new replacement worker:
  __cilkrts_worker* fresh_worker;

#ifdef CILK_IVARS_CACHING
  if((fresh_worker = (__cilkrts_worker *) dequeue_paused_stack(w->worker_cache)) == NULL){
    fresh_worker = (__cilkrts_worker*)__cilkrts_malloc(sizeof(__cilkrts_worker)); 
    fresh_worker->reference_count = 0;
  } else {
    IVAR_DBG_PRINT_(1,"[concurrent-cilk] got new CACHED worker %d/%p \n",fresh_worker->self, fresh_worker);
    CILK_ASSERT(w->cached == 1);
    w->cached = 0;

    if_f(fresh_worker->reference_count != 0) {

      IVAR_DBG_PRINT_(1,"[concurrent-cilk] worker %d/%p was still referenced. MALLOCING instead\n",fresh_worker->self, fresh_worker);
      enqueue_paused_stack(w->worker_cache,fresh_worker);
      fresh_worker = (__cilkrts_worker*)__cilkrts_malloc(sizeof(__cilkrts_worker)); 
      fresh_worker->reference_count = 0;
    } else {
      __cilkrts_worker_unlock(fresh_worker);
    }

  }
#else
  fresh_worker = (__cilkrts_worker*)__cilkrts_malloc(sizeof(__cilkrts_worker)); 
#endif

  CILK_ASSERT(fresh_worker);
  __cilkrts_worker* new_w = (__cilkrts_worker *) replace_worker(w, fresh_worker, stk);

  // mark the new worker as a replacement:
  new_w->is_replacement = 1;

  IVAR_DBG_PRINT_(1," [concurrent-cilk,finalize pause] Pause Finalized, new worker %d/%p (from %p), replaced blocked worker %d/%p.\n", 
      new_w->self, new_w, new_w->l->frame_ff, w->self,w);

  // increment the number of paused stacks in existence
  __sync_fetch_and_add(&w->g->num_paused_stacks,1);

  //release the lock on the stack
  IVAR_DBG_PRINT_(1," [concurrent-cilk,finalize pause] Pause Finalized, new worker %d/%p (from %p), replaced blocked worker %d/%p.\n", 
      new_w->self, new_w, new_w->l->frame_ff, w->self,w);

  while(!paused_stack_unlock(w,stk))
    __cilkrts_short_pause();

  // head to the scheduler with the replacement worker
  setup_and_invoke_scheduler(new_w);
  CILK_ASSERT(0); //no return
}

// Back out of the pause before making any externally visible changes.
// The client better not have stored the __cilkrts_paused_stack anywhere!
CILK_API(void) __cilkrts_undo_pause(__cilkrts_worker* w, volatile __cilkrts_paused_stack* stk) 
{
  CILK_ASSERT(stk);
  CILK_ASSERT(stk->orig_worker);

  BEGIN_WITH_WORKER_LOCK((__cilkrts_worker *) stk->orig_worker) {
    stk->orig_worker->forwarding_pointer = NULL;
  } END_WITH_WORKER_LOCK((__cilkrts_worker *) stk->orig_worker);

  __cilkrts_free((void *) stk);
  IVAR_DBG_PRINT_(2," [concurrent-cilk, undo_pause] Pause of stk %p undone.\n", stk);
}

// This function is a `yield` for defining coroutines.  It stops the current computation but adds it
// to a ready queue to be awoken after other tasks are given a chance to run.
CILK_API(void) __cilkrts_pause_a_bit(struct __cilkrts_worker* w)
{

  // Here we do not need to save anything... the worker is left in place, but the thread abandons it.
  // Save the continuation in the top stack frame of the old (stalled) worker:
  volatile struct __cilkrts_paused_stack* ptr = __cilkrts_pause(w);
  IVAR_DBG_PRINT_(1," [scheduler: pause_a_bit] Creating paused stack: %p capturing current stack %p\n", ptr, w->l->frame_ff->stack_self);
  if(ptr) {
    __cilkrts_wake_stack(ptr);
    __cilkrts_finalize_pause(w,ptr);
  }
}

// Mark a paused stack as ready and add it to the ready queue.
CILK_API(void) __cilkrts_wake_stack(volatile __cilkrts_paused_stack* stk)
{
  CILK_ASSERT(stk);
  IVAR_DBG_PRINT_(2," [concurrent-cilk, wake_stack] Attempting wakeup on stack %p (orig_worker %p)...\n", stk, stk->orig_worker);
  // Waking the stack up means moving it to the ready queue.
  if_t ( __sync_bool_compare_and_swap(& stk->ready, 0, 1 ) ) {
    // Multiple [concurrent] calls to __cilkrts_wake_stack are idempotent.
    // If we are the one that gets there first we are responsible for adding the stack
    // to the ready queue.
    __cilkrts_worker* w = (__cilkrts_worker *) stk->orig_worker;
    CILK_ASSERT(w);

    IVAR_DBG_PRINT_(3," [concurrent-cilk, wake_stack]   We got there first, put stk %p on ready queue of worker.\n", stk);
#ifdef CACHE_AWARE_QUEUE
    enqueue_paused_stack(w->paused_but_ready_stacks, (void *) stk);
#else
    enqueue_paused_stack(w->g->paused_but_ready_stacks, (void *) stk);
#endif


    IVAR_DBG_PRINT_(1," [concurrent-cilk, wake_stack]   Woke stack %p by putting it in ready queue.\n", stk);
    return;
  } else {
    IVAR_DBG_PRINT_(2," [concurrent-cilk, wake_stack] FAILED to wakeup stack %p; did someone else do it?\n", stk);
    return;
  }
}

// Helper used for debugging:
void __cilkrts_show_threadid()
{
  pthread_t id = pthread_self();
  fprintf(stderr, "TID %lu ", (unsigned long)id);
}

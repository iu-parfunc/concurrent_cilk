
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

#include "lockfree_queue.h"
#include "../concurrent_cilk_internal.h"
#include "../cilk_malloc.h"

//Michael & Scott Lockfree Queues

__cilkrts_stack_queue* make_stack_queue() {

  __cilkrts_stack_queue* q = 
    (__cilkrts_stack_queue*)__cilkrts_malloc(sizeof(__cilkrts_stack_queue));
  // INVARIANT: There is always at least one pair object.  Head/tail always have something to point at.
  // Therefore, create an INVALID, CORRUPT initial element (data uninitialized):
   __cilkrts_stack_pair* newp = 
    ( __cilkrts_stack_pair*)__cilkrts_malloc(sizeof(__cilkrts_stack_pair));
  newp->data = NULL;
  newp->next = NULL;
  q->head = newp;
  q->tail = newp;
  return q;
}

// Allocate space and add a new element.
void enqueue_paused_stack(__cilkrts_stack_queue* q, void* stk) {

  __cilkrts_stack_pair *tail, *next, *newp;
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Begin enqueue of stack %p into queue %p\n", stk, q);
  newp        = (__cilkrts_stack_pair*)__cilkrts_malloc(sizeof(__cilkrts_stack_pair)); 
  newp->data  = stk;
  newp->next  = NULL;
  while(1) {
    tail = q->tail;
    next = q->tail->next;

    if (tail == q->tail)
      if (next == NULL) {
        if ( __sync_bool_compare_and_swap( &tail->next, next, newp) )
          break;
      } else {
        // Best effort bump of the tail:
        __sync_bool_compare_and_swap( &q->tail, tail, next );
      }
  }
  // If we fail to swing the tail that is ok.  Whoever came in after us deserves it.
  __sync_bool_compare_and_swap( &q->tail, tail, newp );
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Successfully enqueued stalled stack %p in queue %p\n", stk, q);
}

// Returns NULL if the queue appeared empty:
void* dequeue_paused_stack(__cilkrts_stack_queue* q) {

  volatile __cilkrts_stack_pair *head; //this needs to be volatile, or it may have incorrect state
  __cilkrts_stack_pair *tail, *next;
  IVAR_DBG_PRINT_(4, " [concurrent-cilk,lockfree-Q] Begin dequeue from queue %p\n", q);
  void* stk;
  while(1) {
    head = q->head;
    tail = q->tail;
    next = head->next;
    if ( head == q->head )
      if ( head == tail ) {
        if ( next == NULL ) 
          return NULL;
        // Try to advance the tail, which is falling behind:
        __sync_bool_compare_and_swap( &q->tail, tail, next );
      } else {
        stk = next->data; 
        // Try to advance the head:
        if (__sync_bool_compare_and_swap(&q->head, head, next) )
          break;
      }
  }
  __cilkrts_free((__cilkrts_stack_pair *) head);
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Successfully dequeued stalled stack %p from queue %p\n", stk, q);
  return stk;
}

// This is shared across the three implementations below:
void delete_stack_queue(__cilkrts_stack_queue* q) {
  // Remove any entries within the queue:
  while( dequeue_paused_stack(q) ) { }
  // Finally, destroy the struct itself:
  __cilkrts_free(q);
}




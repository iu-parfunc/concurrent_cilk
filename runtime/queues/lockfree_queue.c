
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

queue_t* make_stack_queue() {

  queue_t* q = 
    (queue_t*)__cilkrts_malloc(sizeof(queue_t));
  // INVARIANT: There is always at least one pair object.  Head/tail always have something to point at.
  // Therefore, create an INVALID, CORRUPT initial element (data uninitialized):
  __cilkrts_stack_pair* newp = 
    (__cilkrts_stack_pair*)__cilkrts_malloc(sizeof(__cilkrts_stack_pair));
  newp->data = (ELEMENT_TYPE) NULL;
  newp->next = NULL;
  q->head = newp;
  q->tail = newp;
  return q;
}

// Allocate space and add a new element.
int enqueue(queue_t* q, ELEMENT_TYPE value) 
{
  __cilkrts_stack_pair *tail, *next, *newp;
#ifdef QUEUE_DEBUG
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Begin enqueue of stack %p into queue %p\n", value, q);
#endif
  newp        = (__cilkrts_stack_pair*)__cilkrts_malloc(sizeof(__cilkrts_stack_pair)); 
  newp->data  = value;
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
#ifdef QUEUE_DEBUG
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Successfully enqueued stalled stack %p in queue %p\n", value, q);
#endif
  return SUCCESS;
}

// Returns NULL if the queue appeared empty:
int dequeue(queue_t* q, ELEMENT_TYPE * value)
{
  volatile __cilkrts_stack_pair *head; 
  __cilkrts_stack_pair *tail, *next;
#ifdef QUEUE_DEBUG
  IVAR_DBG_PRINT_(4, " [concurrent-cilk,lockfree-Q] Begin dequeue from queue %p\n", q);
#endif
  while(1) {
    head = q->head;
    tail = q->tail;
    next = head->next;
    if ( head == q->head )
      if ( head == tail ) {
        if ( next == NULL ) 
          return QUEUE_EMPTY;
        // Try to advance the tail, which is falling behind:
        __sync_bool_compare_and_swap( &q->tail, tail, next );
      } else {
        *value = next->data; 
        // Try to advance the head:
        if (__sync_bool_compare_and_swap(&q->head, head, next) )
          break;
      }
  }
  __cilkrts_free((queue_t *) head);
#ifdef QUEUE_DEBUG
  IVAR_DBG_PRINT_(3, " [concurrent-cilk,lockfree-Q] Successfully dequeued stalled stack %p from queue %p\n", *value, q);
#endif
  return SUCCESS;
}

int q_is_empty(queue_t *q) 
{
  volatile queue_t *next, *head;
  head = q->head;
  next = head->next;
  if (head->next == NULL) 
    return QUEUE_EMPTY;
  return SUCCESS;
}

  void delete_stack_queue(queue_t* q) 
  {
    // Remove any entries within the queue:
    ELEMENT_TYPE hukarz;
    while(dequeue(q, &hukarz)); 
    // Finally, destroy the struct itself:
    __cilkrts_free(q);
  }




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

#ifdef  TBB_QUEUE_VERSION
#include "concurrent_queue_tbb.h"
#endif

#include <concurrent_cilk.h>

// The lock-based reference version
// For correctness, it is initially safer to use a locking version:

// Consists of a pair type and a list/queue type.
struct __cilkrts_stack_pair {
  QUEUE_ELEMTY* data;
  struct __cilkrts_stack_pair* next;
};

QUEUE_ELEMTY* dequeue_paused_stack(volatile struct __cilkrts_stack_queue* q);

// This is shared across the three implementations below:
void delete_stack_queue(struct __cilkrts_stack_queue* q) {
  // Remove any entries within the queue:
  while( dequeue_paused_stack(q) ) { }
  // Finally, destroy the struct itself:
  __cilkrts_free(q);
}

//struct __cilkrts_stack_queue_struct {
  struct __cilkrts_stack_pair* head;
  struct __cilkrts_stack_pair* tail;        
  pthread_mutex_t mut; 
};

typedef struct __cilkrts_stack_queue_struct __cilkrts_stack_queue;

__cilkrts_stack_queue* make_stack_queue() 
{
  __cilkrts_stack_queue* q = 
    (__cilkrts_stack_queue*)__cilkrts_malloc(sizeof(__cilkrts_stack_queue));

  pthread_mutex_init( &q->mut, NULL);
  q->head = NULL;
  q->tail = NULL;
  return q;
}

void enqueue_paused_stack(__cilkrts_stack_queue* q, QUEUE_ELEMTY* stk) 
{
  struct __cilkrts_stack_pair * newp = 
    (struct __cilkrts_stack_pair*)__cilkrts_malloc(sizeof(struct __cilkrts_stack_pair)); 

  IVAR_DBG_PRINT_(3, " [concurrent-cilk,mutex-queue] Begin enqueue of stack %p into queue %p\n", stk, q);
  newp->data  = stk;
  newp->next  = NULL;
  pthread_mutex_lock(&q->mut);
  if (q->tail == NULL)
    q->head = q->tail = newp;
  else {
    q->tail->next = newp;
    q->tail      = newp;
  }
  pthread_mutex_unlock(&q->mut);
  IVAR_DBG_PRINT_(2, " [concurrent-cilk,mutex-queue]   Successfully enqueued stalled stack %p in queue %p\n", stk, q);
}

// Returns NULL if the queue appeared empty:
QUEUE_ELEMTY* dequeue_paused_stack(volatile __cilkrts_stack_queue* q)
{
  QUEUE_ELEMTY* stk;
  struct __cilkrts_stack_pair* pr;
  IVAR_DBG_PRINT_(4, " [concurrent-cilk,mutex-queue] Begin dequeue from queue %p\n", q);
  pthread_mutex_lock((pthread_mutex_t*) &q->mut);
  if (q->head == NULL) { 
    pthread_mutex_unlock((pthread_mutex_t*) &q->mut);
    return NULL;
  } else {
    pr = q->head;
    q->head = q->head->next;
    if (q->head == NULL) q->tail = NULL;
  }
  pthread_mutex_unlock((pthread_mutex_t*) &q->mut);
  stk = pr->data;
  __cilkrts_free(pr);
  IVAR_DBG_PRINT_(3," [concurrent-cilk,mutex-queue] Successfully dequeued stalled stack %p from queue %p\n", stk, q);
  return stk;
}


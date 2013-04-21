#include <stdio.h>
#include <stdlib.h>
#include "lockfree_queue.h"

// Michael and Scott Lockfree queue
queue_struct* make_stack_queue() {

  queue_struct* q = (queue_struct*) malloc(sizeof(queue_struct));
  // INVARIANT: There is always at least one pair object.  Head/tail always have something to point at.
  // Therefore, create an INVALID, CORRUPT initial element (data uninitialized):
  struct pair* newp = (struct pair *) malloc(sizeof(struct pair));
  newp->data = NULL;
  newp->next = NULL;
  q->head = (queue_struct *) newp;
  q->tail = (queue_struct *) newp;
  return q;
}

// Allocate space and add a new element.
void enqueue(volatile queue_struct* q, QUEUE_ELEMTY* stk) {

  struct pair *tail, *next, *newp;
  newp        = (struct pair*) malloc(sizeof(struct pair)); 
  newp->data  = stk;
  newp->next  = NULL;
  while(1) {
    tail = (struct pair *) q->tail;
    next = ((struct pair *) q->tail)->next;

    if (tail == (struct pair *) q->tail)
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
}

// Returns NULL if the queue appeared empty:
QUEUE_ELEMTY* dequeue(volatile queue_struct* q) {

  volatile struct pair *head; //this needs to be volatile, or it may have incorrect state
  struct pair *tail, *next;
  QUEUE_ELEMTY* stk;
  while(1) {
    head = (struct pair *) q->head;
    tail = (struct pair *) q->tail;
    next = head->next;
    if ( head == (struct pair *) q->head )
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
  free((struct pair *) head);
  return stk;
}



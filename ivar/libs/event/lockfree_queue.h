#ifndef _lockfree_qeue_h
#define _lockfree_qeue_h
// Michael and Scott Lockfree queue
//
// this can be overridden in the C file that the queue is initialized to be whatever you want
#define QUEUE_ELEMTY void

struct pair {
  QUEUE_ELEMTY *data;
  struct pair  *next;
};

typedef struct queue_struct {
  struct queue_struct *head;
  struct queue_struct *tail;
} queue_struct;

queue_struct* make_stack_queue(void);
void enqueue(volatile queue_struct* q, QUEUE_ELEMTY* stk);
QUEUE_ELEMTY* dequeue(volatile queue_struct* q);
#endif

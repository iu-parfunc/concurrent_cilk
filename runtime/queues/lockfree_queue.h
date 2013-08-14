
#ifndef __LOCKFREE_QUEUE_H 
#define __LOCKFREE_QUEUE_H 

#include <cilk/common.h>
#include "../concurrent_cilk_internal.h"

__CILKRTS_BEGIN_EXTERN_C

#define ELEMENT_TYPE uint64_t
#define SUCCESS 1
#define QUEUE_EMPTY 0

// Consists of a pair type and a list/queue type.
struct __cilkrts_stack_pair {
  ELEMENT_TYPE data;
  struct __cilkrts_stack_pair* next;
};


struct queue_t {
  struct __cilkrts_stack_pair* head;
  struct __cilkrts_stack_pair* tail;        
}; //maybe aligne this?

typedef struct queue_t queue_t;

queue_t* make_stack_queue();
void delete_stack_queue(queue_t* q);
int enqueue(queue_t *q, ELEMENT_TYPE value);
int dequeue(queue_t *q, ELEMENT_TYPE *value);
int q_is_empty(queue_t *q);

__CILKRTS_END_EXTERN_C
#endif

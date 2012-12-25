
#ifndef __LOCKFREE_QUEUE_H 
#define __LOCKFREE_QUEUE_H 

#include <cilk/common.h>
#include <stdio.h>

__CILKRTS_BEGIN_EXTERN_C

#define IVAR_DBG_PRINT_(lvl, ...) if(IVAR_DBG >= lvl) {    \
  pthread_t id = pthread_self(); char buf[512];             \
  sprintf(buf, __VA_ARGS__);                                \
  volatile struct __cilkrts_worker* tw = __cilkrts_get_tls_worker(); \
  fprintf(stderr, "[tid/W %3d %2d/%p] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, tw, buf); }


// Consists of a pair type and a list/queue type.
struct __cilkrts_stack_pair {
  void* data;
  struct __cilkrts_stack_pair* next;
};


struct __cilkrts_stack_queue_struct {
  struct __cilkrts_stack_pair* head;
  struct __cilkrts_stack_pair* tail;        
};

typedef struct __cilkrts_stack_queue_struct __cilkrts_stack_queue;

__cilkrts_stack_queue* make_stack_queue();
void delete_stack_queue(__cilkrts_stack_queue* q);
void  enqueue_paused_stack(__cilkrts_stack_queue* q, void* stk);
void* dequeue_paused_stack(__cilkrts_stack_queue* q);

__CILKRTS_END_EXTERN_C
#endif

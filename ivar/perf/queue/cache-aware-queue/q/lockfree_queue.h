#ifndef lockfree_queue_h
#define lockfree_queue_h

#include <limits.h>
#include <stdlib.h>

#define BLOCK_SIZE 128
#define MAX_NODES 1000000
#define NULL2 0x02
#define QUEUE_ELEMTY void

typedef struct node_t {
  QUEUE_ELEMTY *data;
  volatile struct node_t *next;
} node_t;
 
typedef struct block_t {
  node_t *nodes[BLOCK_SIZE];
  volatile int refcount;
  int head;
  int tail;
  int deleted;
  struct block_t *next;
} block_t;

typedef struct local_block_t {
  // Thread-local storage
  block_t *threadHeadBlock;
  block_t *threadTailBlock;
  int threadHead;
  int threadTail;
} local_block_t;

block_t nodes[MAX_NODES];
block_t *volatile freeList;

// Shared variables
block_t *globalHeadBlock;
block_t *globalTailBlock;

block_t *new_node();
void init_queue();
local_block_t *init_thread();
void enqueue(local_block_t *l, QUEUE_ELEMTY *item);
QUEUE_ELEMTY *dequeue(local_block_t *l);
void DeleteNode(block_t *node);

block_t *DeRefLink(block_t *volatile *link);
void release_block(block_t *node);
int atomic_update(block_t **link, block_t *node);


#endif

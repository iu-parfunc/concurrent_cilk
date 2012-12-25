
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

#define IVAR_DBG_PRINT_(lvl, ...) if(IVAR_DBG >= lvl) {    \
  pthread_t id = pthread_self(); char buf[512];             \
  sprintf(buf, __VA_ARGS__);                                \
  volatile struct __cilkrts_worker* tw = __cilkrts_get_tls_worker(); \
  fprintf(stderr, "[tid/W %3d %2d/%p] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, tw, buf); }

#include "concurrent_cilk.h"

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

// Tsigas, Sundell and Gidenstam lockfree cache aware queue
// http://www.cse.chalmers.se/~tsigas/papers/Cache-Aware-Queues-OPODIS10.pdf

#define BLOCK_SIZE 128
#define NULL2 0x02
void init_queue();


struct block_t {
  struct __cilkrts_stack_pair *nodes[BLOCK_SIZE];
  volatile int refcount;
  int head;
  int tail;
  int deleted;
  struct block_t *next;
};

struct __cilkrts_stack_queue_struct {
  // Thread-local storage
  struct block_t *threadHeadBlock;
  struct block_t *threadTailBlock;
  int threadHead;
  int threadTail;
};

typedef struct __cilkrts_stack_queue_struct __cilkrts_stack_queue;

typedef struct block_t block_t;

__cilkrts_stack_queue *make_stack_queue();
block_t *deref_link(block_t *volatile *link);
block_t *new_node();

// cache the blocks
block_t *volatile freeList;

// Shared variables
block_t *globalHeadBlock;
block_t *globalTailBlock;

__cilkrts_stack_queue *init_thread();

__cilkrts_stack_queue *make_stack_queue() {
  if(globalTailBlock == NULL) init_queue();
  return init_thread();
}

int atomic_update(block_t **ptr, block_t *val) {
  return __sync_bool_compare_and_swap(ptr,ptr,val);
}

block_t *new_block() {
  block_t *block = new_node();
  block->next = NULL;
  block->head = 0;
  block->tail = 0;
  block->deleted = 0;
  int i;
  for(i=0;i<BLOCK_SIZE;i++) block->nodes[i]=NULL;
  return block;
}

void init_queue() {
  block_t *block = new_block();
  globalTailBlock = block;
  globalHeadBlock = block;
}

__cilkrts_stack_queue *init_thread() {
  __cilkrts_stack_queue *l = (__cilkrts_stack_queue *) calloc(1,sizeof(__cilkrts_stack_queue));

  l->threadHeadBlock = deref_link(&globalHeadBlock);
  l->threadTailBlock = deref_link(&globalTailBlock);
  l->threadHead = l->threadHeadBlock->head;
  l->threadTail = l->threadTailBlock->tail;
  return l;
}

void enqueue_paused_stack(__cilkrts_stack_queue *l, QUEUE_ELEMTY *item) {

  int head = l->threadHead;
  block_t *block = l->threadHeadBlock;
  for(;;) {
    if(head==BLOCK_SIZE) {
      block_t *oldBlock = block;
      block->head = head;
      block = deref_link(&block->next);
      if(block == NULL) {
        block = (block_t *) new_block();
        while(globalHeadBlock != oldBlock && oldBlock->next==NULL) {
          block_t *headBlock = deref_link(&globalHeadBlock);
          if(headBlock->next != oldBlock) break;
          if(__sync_bool_compare_and_swap(&globalHeadBlock,headBlock,oldBlock)) break;
        }
        if(__sync_bool_compare_and_swap(&oldBlock->next,NULL,block))
          __sync_bool_compare_and_swap(&globalHeadBlock,oldBlock,block);
        else {
          release_block(block);
          block = deref_link(&oldBlock->next);
        }
      }
      else if(block->head==BLOCK_SIZE && block->next!=NULL)
        block = deref_link(&globalHeadBlock);
      l->threadHeadBlock = block;
      head = block->head;
    }
    else if(block->nodes[head]==NULL) {
      if(__sync_bool_compare_and_swap(&block->nodes[head],NULL,item)) {
        l->threadHead = head+1;
        return;
      }
    }
    else head++;
  }
}

QUEUE_ELEMTY *dequeue_paused_stack(volatile __cilkrts_stack_queue *l) {
  int tail = l->threadTail;
  block_t *block = l->threadTailBlock;
  for(;;) {
    if(tail==BLOCK_SIZE) {
      block_t *oldBlock = block;
      block->tail = tail;
      block=deref_link(&block->next);
      if(block == NULL)
        return NULL;
      else {
        if(!oldBlock->deleted) {
          while(globalTailBlock != oldBlock && !oldBlock->deleted) {
            block_t *tailBlock= deref_link(&globalTailBlock);
            if(tailBlock->next != oldBlock) continue;
            if(__sync_bool_compare_and_swap(&globalTailBlock,tailBlock,oldBlock))
              release_block(tailBlock);
          }
          if(__sync_bool_compare_and_swap(&oldBlock->deleted,0,1)) {
            if(__sync_bool_compare_and_swap(&globalTailBlock,oldBlock,block))
              release_block(oldBlock);
          }
        }
        if(block->deleted)
          block=deref_link(&globalTailBlock);
      }
      l->threadTailBlock = block;
      tail = block->tail;
    }
    else {
      void *data = block->nodes[tail];
      if(data== (void *) NULL2)
        tail++;
      else if(data==NULL && __sync_bool_compare_and_swap(&block->nodes[tail],NULL,NULL)) {
        l->threadTail = tail;
        return NULL;
      }
      else if(__sync_bool_compare_and_swap(&block->nodes[tail],data,NULL2)) {
        l->threadTail = tail+1;
        return data;
      }
    }
  }
}


block_t *new_node() {
  for(;;) {
    block_t *p = freeList;
    if(!p) p = calloc(1, sizeof(block_t));
    __sync_fetch_and_add(&(p->refcount), 2);
    if(__sync_bool_compare_and_swap(&freeList,p,p->next)) {
      __sync_fetch_and_add(&(p->refcount),-1);
      return p;
    }
    release_block(p);
  }
}

void release_block(block_t *node) {
  __sync_fetch_and_add(&(node->refcount),-2);
  if(__sync_bool_compare_and_swap(&node->refcount,0,1)) {
    if(node->next) release_block(node->next);
    block_t *q; do {
      q = freeList;
      node->next = q;
    } while(!__sync_bool_compare_and_swap(&freeList,q,node));
  }
}

block_t *deref_link(block_t *volatile *link) {
  for(;;) {
    block_t *q = *link;
    if(q == NULL) return q;
    __sync_fetch_and_add(&(q->refcount), 2);
    if(q == *link) return q;
    release_block(q); 
  }
}


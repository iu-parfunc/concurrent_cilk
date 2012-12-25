#include "lockfree_queue.h"
#include <stdlib.h>


void terminate_node(block_t *node) {
  atomic_update(&node->next,NULL);
}

int atomic_update(block_t **ptr, block_t *val) {
  return __sync_bool_compare_and_swap(ptr,ptr,val);
}

void cleanup_node(block_t *node) {
  block_t *next = DeRefLink(&node->next);
  block_t *next2 = DeRefLink(&globalTailBlock);
  __sync_bool_compare_and_swap(&node->next, next, next2);
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

local_block_t *init_thread() {
  local_block_t *l = (local_block_t *) calloc(1,sizeof(local_block_t));

  l->threadHeadBlock = DeRefLink(&globalHeadBlock);
  l->threadTailBlock = DeRefLink(&globalTailBlock);
  l->threadHead = l->threadHeadBlock->head;
  l->threadTail = l->threadTailBlock->tail;
  return l;
}

void enqueue(local_block_t *l, QUEUE_ELEMTY *item) {

  int head = l->threadHead;
  block_t *block = l->threadHeadBlock;
  for(;;) {
    if(head==BLOCK_SIZE) {
      block_t *oldBlock = block;
      block->head = head;
      block = DeRefLink(&block->next);
      if(block == NULL) {
        block = (block_t *) new_block();
        while(globalHeadBlock != oldBlock && oldBlock->next==NULL) {
          block_t *headBlock = DeRefLink(&globalHeadBlock);
          if(headBlock->next != oldBlock) break;
          if(__sync_bool_compare_and_swap(&globalHeadBlock,headBlock,oldBlock)) break;
        }
        if(__sync_bool_compare_and_swap(&oldBlock->next,NULL,block))
          __sync_bool_compare_and_swap(&globalHeadBlock,oldBlock,block);
        else {
          release_block(block);
          block = DeRefLink(&oldBlock->next);
        }
      }
      else if(block->head==BLOCK_SIZE && block->next!=NULL)
        block = DeRefLink(&globalHeadBlock);
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

QUEUE_ELEMTY *dequeue(local_block_t *l) {
  int tail = l->threadTail;
  block_t *block = l->threadTailBlock;
  for(;;) {
    if(tail==BLOCK_SIZE) {
      block_t *oldBlock = block;
      block->tail = tail;
      block=DeRefLink(&block->next);
      if(block == NULL)
        return NULL;
      else {
        if(!oldBlock->deleted) {
          while(globalTailBlock != oldBlock && !oldBlock->deleted) {
            block_t *tailBlock= DeRefLink(&globalTailBlock);
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
          block=DeRefLink(&globalTailBlock);
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

block_t *DeRefLink(block_t *volatile *link) {
  for(;;) {
    block_t *q = *link;
    if(q == NULL) return q;
    __sync_fetch_and_add(&(q->refcount), 2);
    if(q == *link) return q;
    release_block(q); 
  }
}



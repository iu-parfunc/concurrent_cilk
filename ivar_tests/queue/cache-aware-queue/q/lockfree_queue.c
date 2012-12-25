#include "lockfree_queue.h"
#include <stdlib.h>
#include "getcache.h"



/*



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




// WRONG
void terminate_node(block_t *node) {
  atomic_update(&node->next,NULL);
}
// WRONG
int atomic_update(block_t **ptr, block_t *val) {
  return __sync_bool_compare_and_swap(ptr,ptr,val);
}

// WRONG
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
*/
// BEGIN PROGRAM 1: from http://www.cse.chalmers.se/~tsigas/papers/LFMemRec-TPDS.pdf

/* HAVE TO DECLARE:
 *
 * NR_INDICIES 
 * NR_THREADS 
 * THRESHOLD_C
 * THRESHOLD_S
 * NR_LINKS_NODE
 *
*/

typedef struct {
  int mm_ref= 0;
  bool mm_trace = 0;
  bool mm_del = 0;
  // add in more here if we want
  node_t *next[NR_LINKS_NODE] = NULL; // want to make NR_LINKS_NODE to be such that we hit cache lines 
} node_t;

node_t* HP[NR_THREADS][NR_INDICIES];
node_t* DL_nodes[NR_THREADS][THRESHOLD_C];
int DL_claims[NR_THREADS][THRESHOLD_C];
bool DL_done[NR_THREADS][THRESHOLD_C];

// local static vairables
int threadId;
int dlist;
int dcount;
int DL_nexts[THRESHOLD_C];

// local temporary variables
node_t* node;
node_t* node1;
node_t* node2;
node_t* old;
int thread, indexm new_dlist, new_dcount;
node_t* plist[];

// should be correct....
int findNullIndex(){
  int i;
  for(i = 0; i < NR_LINKS_NODE; i++){
    if(HP[threadId][i] = NULL){
      return i;
      break; 
    } else { 
      continue;
    }
  }
}

int getNodeIndex(node_t *node){
  int i;
  for(i = 0; i < NR_LINKS_NODE; i++){
    if(HP[threadId][i] = node){
      return i;
      break; 
    } else { 
      continue;
    }
  }
}


// PROGRAM 3
node_t* DeRefLink(node_t **link){
  int index = findNullIndex(); // own function. CREATE THIS  
while(1){
    node_t node;
    node = *link;
    HP[threadId][index] = node;
    if(*link = node)
      return node;
  }
}

void ReleaseRef(node_t *node){
  int i = getNodeIndex(node);
  HP[threadId][i] = NULL;
}

bool CASRef(node_t **link, node_t *old, node_t *new) {
  if(__sync_bool_compare_and_swap(link, old, new)){
    if(new != NULL){
      __sync_fetch_and_add(&new.mm_ref, 1);
      new.mm_trace = 0;
    } else {
      if(old != NULL) {
        __sync_fetch_and_add(&old.mm_ref, -1);
        return 1;
      }
    }
  } 
    return 0;
}


void StoreRef(nodet_t **link, node_t *node){
  node_t old = *link;
  *link = node;
  if(node != NULL){
    __sync_fetch_and_add(&node.mm_ref,1);
    node.mm_trace = 0;
  } 
  if(old != NULL)
    __sync_fetch_and_add(&node.mm_ref, -1);
}

node_t *NewNode(int size){
  node_t *node = calloc(1, size);  //calloc(1,sizeof(node_t));
  node.mm_ref = 0;
  node.mm_trace = 0;
  node.mm_del = 0;
  int i = findNullIndex();
  HP[threadId][i] = node;

  return node;
}

void DeleteNode(node_t *node){

  ReleaseRef(node);
  node.mm_del = 1;
  node.mm_trace = 0;
  int i = findNullIndex_DL(); // add
  DL_done[threadId][i] = 0;
  DL_nodes[threadId][i] = node;
  DL_nexts[i] = dlist;
  dlist = i;
  dcount += 1;
  for(;;){
    // following functions are in program 5 of the paper
  if(dcount = THRESHOLD_C) CleanUpLocal();
  if(dcount >= THRESHOLD_S) Scan();// add in
  if(dcount = THRESHOLD_C) CleanUpAll();
  
  
  
  }

}


// SUBPROGRAM 5

void CleanUpLocal(){
  int i = dlist;
  while(i != NULL){ // bottom...
    node_t node = DL_nodes[threadId][i];
    CleanUpNode(node);
    i = DL_nexts[index];
  }
}

void CleanUpAll(){
  int thread, j;
  for(thread = 0; thread < NR_THREADS - 1; thread++){
    for(index = 0; index < THRESHOLD_C - 1; index++){
      node_t node = DL_nodes[thread][index];
      if(node != NULL && !(DL_done[thread][index])){
        __sync_fetch_and_add(&DL_claims[thread][index], 1);
        if(node = DL_nodes[thread][index])
          CleanUpNode(node);
        __sync_fetch_and_add(&DL_claims[thread][index], -1;)
      }
    }
  }
}


void Scan(){

  int index = dlist;
  while(index != NULL){
    node_t node = DL_nodes[threadId][index];
    if(node.mm_ref = 0){

      node.mm_trace = 1;
      if(node.mm_ref != 0){
        node.mm_trace = 0;
      }
    } else index = DL_nexts[index];
  }
  plist = NULL;
  int new_dlist = 0;
  int new_dcount = 0;
  int thread, index;
  for(thread = 0; thread < NR_THREADS - 1; thread++){

    for(index = 0; index < NR_INDICIES - 1; index++){

      node = HP[thread][index];
      if(node != NULL)
        plist = plist + node;// have to figure this out
    }
  }
  // sort and remove duplicates in array plist
  while(dlist != 0){

    index = dlist;
    node = DL_nodes[threadId][index];
    dlist = DL_nexts[index];
    if(node.mm_ref = 0 && node.mm_trace && notIn(node)) {//add

      DL_nodes[threadId][index] = NULL;
      if(DL_claims[threadId][index] = 0){

        TerminateNode1(node, 0);
        // free the memory of the node
        continue;
      }
      TerminateNode1(node, 1);
      DL_done[threadId][index] = 1;
      DL_nodes[threadId][index] = node;
    }
    DL_nexts[index] = new_dlist;
    new_dlist = index;
    new_dcount += 1;
  }
  dlist = new_dlist;
  dcount = new_dcount;
}
// similar to TerminateNode, except this time we need a boolean to tell us if were concurrent or not (for speed purposes)
// FIX THIS. Next is an array...
void TerminateNode1(node_t *node, bool b){

  node_t node1;
  if(!b){
    while(node.next != NULL){
      if(node.next.mm_ref){
        StoreRef(node.next, NULL);
        node = node.next;
      } else node = node.next;
    }
  } else {
    while(node.next != NULL){
      if(node.next.mm_ref){
        while(!CASRef(&node.next, node1, NULL));
        node1 = node.next;
      }
      node = node.next;
    }
  }
}


// END PROGRAM 1

// PROGRAM 2: from http://www.cse.chalmers.se/~tsigas/papers/Cache-Aware-Queues-OPODIS10.pdf
void TerminateNode(block_t *node) {
  StoreRef(&node->next,NULL);
}

void CleanUpNode(block_t *node) {
  block_t *next = DeRefLink(&node->next);
  block_t *next2 = DeRefLink(&globalTailBlock);
  CASRef(&node->next, next, next2);
}


// PROGRAM 3
typedef struct {
  void *nodes[BLOCK_SIZE]; // and BLOCK_SIZE is ....
  int head;
  int tail;
  bool deleted;
  block_t *next;
} block_t;

block_t * NewBlock() {

  block_t * block = NewNode(sizeof(block_t));
  block->next = NULL;
  block->head = 0;
  block->tail = 0;
  block->deleted = false;
  for(int i=0;i<BLOCK_SIZE;i++) block->nodes[i]=NULL;
  return block;
}

void InitQueue() {
  block_t * block = NewBlock();
  StoreRef(&globalHeadBlock,block);
  StoreRef(&globalTailBlock,block);
}

void InitThread() {
  threadHeadBlock = DeRefLink(&globalHeadBlock);
  threadTailBlock = DeRefLink(&globalTailBlock);
  threadHead = threadHeadBlock->head;
  threadTail = threadTailBlock->tail;
}

// Shared variables
block_t * globalHeadBlock, globalTailBlock;

// Thread-local storage
block_t * threadHeadBlock, threadTailBlock;
int threadHead, threadTail;






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


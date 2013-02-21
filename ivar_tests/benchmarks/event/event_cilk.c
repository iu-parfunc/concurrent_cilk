#include "event_cilk.h"
#include "lockfree_queue.h"
#include <stdio.h>
#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

typedef struct pair pair;

static volatile event** events;
static volatile queue_struct *free_q;

int event_init() {

  events = (volatile event **) malloc(MAXEVENTS * sizeof(event *));
  int size = MAXEVENTS-1;

  if(events <= 0) 
    perror("event init: could not create event array");

  free_q = make_stack_queue();
  if(free_q <= 0) 
    perror("event init: could not create free_q");

  for(size; size >= 0; size--)  {
    event *e = (event *) malloc(sizeof(event));
    e->self  = size;
    e->ndeps = 0;
    e->q = make_stack_queue();
    memset(&e->deps,0,MAXDEPS);
    events[size] = e;
    enqueue(free_q, (QUEUE_ELEMTY *) e);
  }

  return 0;
}

event* event_create() {
  event *e = (event *) dequeue(free_q);
  if(e) {
    __cilkrts_ivar_clear(&e->iv);
    return e;
  }
  return NULL;
}

void event_destroy(event *e) {
  if(e->q->head != e->q->tail)
    perror("queue is not in a good state in destroy!");

  pair *head = (pair *) e->q->head; 
  head->data = NULL;
  head->next = NULL;
  memset(&e->deps,0,MAXDEPS);
  __asm__("mfence");
  enqueue(free_q, e);
}

int event_ctl(int eid1, int eid2, ctl_t op) {
  event *e1 = (event *) events[eid1];
  event *e2 = (event *) events[eid2];

  if(e1 == e2) {
    perror("ERROR: trying to add event to itself");
    return -EINVAL;
  }
    
   pair *node, *last_node = NULL;
  switch (op) {
    case ADD: 
      __sync_add_and_fetch(&(e1->ndeps), 1);
      //printf("adding %p\n", e2);
      enqueue(e1->q, e2);
      return 0;
    case DEL:
      __sync_sub_and_fetch(&(e1->ndeps), 1);

      event *etmp;
      while ((etmp = dequeue(e1->q)) != NULL) {
        if (etmp == e2) {
      //    printf("removing %p\n", etmp);
          return 0;
        }
        enqueue(e1->q, etmp);
      }
        return 0;
    default:
      return -EINVAL;
  }
}

int event_fire(int eid, event_data_t *data){
  event *e = (event *) events[eid];
  if(e == NULL)
    return -EINVAL;

  printf("writing %p\n", e);
  __cilkrts_ivar_write(&e->iv, (ivar_payload_t) data);
  return 0;
}

ivar_payload_t event_wait(int eid) {
  volatile __cilkrts_ivar *iv;
  ivar_payload_t dep;
  ivar_payload_t val;
  event *e = (event *) events[eid];
  event *etmp;
  int i;
  printf("e in event wait: %p\n",e);

  //does this need to spawn?
  val =  __cilkrts_ivar_read(&(e->iv));

  for(i = 0; (etmp = dequeue(e->q)) != NULL; i++) {
   // printf("reading %p\n", etmp);
    dep = __cilkrts_ivar_read(&(etmp->iv));
    e->deps[i] = dep;
  }

  return val;
}

ivar_payload_t event_wait_nodeps(int eid) {
  ivar_payload_t val;
  event *e = (event *) events[eid];
  val =  __cilkrts_ivar_read(&(e->iv));
  return val;
}

event* get_event(int eid) {
  return (event *) events[eid];
}

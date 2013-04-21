#ifndef _event_cilk_h_
#define _event_cilk_h_

#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include "lockfree_queue.h"
#include <cilk/cilk_api.h>
#include <cilk/concurrent_cilk.h>

#define MAXEVENTS 1000
#define MAXDEPS 100

typedef enum {ADD, DEL} ctl_t;

typedef union event_data_t {
  void*     ptr;
  uint32_t  u32;
  uint64_t  u64;
} event_data_t;

typedef struct {
  __cilkrts_ivar     iv;
  uint32_t           self;
  volatile uint32_t  ndeps;
  ivar_payload_t deps[MAXDEPS];
  struct queue_struct *q;
} event;

int event_init();
event* event_create();
int event_ctl(int eid1, int eid2, ctl_t op);
int event_fire(int eid, event_data_t *data);
ivar_payload_t event_wait(int eid);
ivar_payload_t event_wait_nodeps(int eid);
void destroy_event(event *e);
event* get_event(int eid);
#endif

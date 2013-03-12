#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>

__CILKRTS_BEGIN_EXTERN_C

typedef struct __cilkrts_coroutine {
  void (*f)(void *);
  void *args;
  struct __cilkrts_worker *slave;
  struct __cilkrts_paused_stack *cont;
} __cilkrts_coroutine;

void __coroutine_run(__cilkrts_coroutine *c);
void yieldto(__cilkrts_coroutine *self, __cilkrts_coroutine *ctx);
__cilkrts_coroutine *new_coroutine(void (*f1), void* f1_args);


typedef unsigned long __cilkrts_ivar;
typedef unsigned long ivar_payload_t;

CILK_API(ivar_payload_t) __cilkrts_ivar_read (__cilkrts_ivar*);
CILK_API(void)           __cilkrts_ivar_write(__cilkrts_ivar*, ivar_payload_t);
CILK_API(void)           __cilkrts_ivar_clear(__cilkrts_ivar*);

typedef struct __cilkrts_paused_stack* PAUSED_FIBER;
CILK_API(void) __cilkrts_finalize_pause(struct __cilkrts_worker* w,  PAUSED_FIBER stk);
CILK_API(void) __cilkrts_undo_pause    (struct __cilkrts_worker* w,  PAUSED_FIBER stk);
CILK_API(void) __cilkrts_wake_stack    (PAUSED_FIBER stk);
CILK_API(void) __cilkrts_pause_a_bit   (struct __cilkrts_worker* w);
CILK_API(void) __cilkrts_msleep(unsigned long millis);
CILK_API(void) __cilkrts_usleep(unsigned long micros);
// CSZ: it is necessary that pause be a macro because the longjump must return to a valid frame. 
// you will experience erratic behavior if this is not the case
#define __cilkrts_pause(w)  (CILK_SETJMP((w->current_stack_frame->ctx))) ?  NULL : make_paused_stack((w)) 


#ifdef IVAR_DBG

/**
 * print out the thread and a message when ivar debug is turned on
 */
#define IVAR_DBG_PRINT_(lvl, ...) if(IVAR_DBG >= lvl) {    \
  pthread_t id = pthread_self(); char buf[512];             \
  sprintf(buf, __VA_ARGS__);                                \
  volatile struct __cilkrts_worker* tw = __cilkrts_get_tls_worker(); \
  fprintf(stderr, "[tid/W %3d %2d/%p] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, tw, buf); }

#else

/**
 * do nothing at all if ivar debug isn't turned on. 
 */
#define IVAR_DBG_PRINT_(lvl, ...)   
#endif

__CILKRTS_END_EXTERN_C
#endif

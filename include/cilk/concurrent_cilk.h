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


typedef long __cilkrts_ivar;
typedef long ivar_payload_t;

typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;
CILK_API(ivar_payload_t) __cilkrts_ivar_read (__cilkrts_ivar*);
CILK_API(void)           __cilkrts_ivar_write(__cilkrts_ivar*, ivar_payload_t);
CILK_API(void)           __cilkrts_ivar_clear(__cilkrts_ivar*);

typedef __cilkrts_ivar *PAUSED_FIBER;
CILK_API(void) __cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack *pstk);
CILK_API(void) __cilkrts_wake_stack    (PAUSED_FIBER stk);
CILK_API(void) __cilkrts_msleep(unsigned long millis);
CILK_API(void) __cilkrts_usleep(unsigned long micros);
int make_paused_stack(__cilkrts_worker* w, __cilkrts_ivar *ivar);
// CSZ: it is necessary that pause be a macro because the longjump must return to a valid frame. 
// you will experience erratic behavior if this is not the case
#define __cilkrts_pause(ctx)  (setjmp((ctx)))

#define IVAR_SHIFT 0x4

#define CILK_IVAR_EMPTY  0x0
#define CILK_IVAR_FULL   0x1
#define CILK_IVAR_PAUSED 0x2

#define IVAR_EMPTY(iv)  (iv == CILK_IVAR_EMPTY)
#define IVAR_READY(iv)  ((iv & 0xf) == CILK_IVAR_FULL)
#define IVAR_PAUSED(iv) ((iv & 0xf) == CILK_IVAR_PAUSED)
#define TAG(iv)   (iv << IVAR_SHIFT)
#define UNTAG(iv) (iv >> IVAR_SHIFT)

/**
 * force this worker to restore all paused computations before proceeding
 */
#define sync_ivars concurrent_sync(NULL)


//convenience aliases for a kinder use of ivars in C
typedef __cilkrts_ivar ivar_t;
#define read_iv  __cilkrts_ivar_read
#define write_iv __cilkrts_ivar_write
#define clear_iv __cilkrts_ivar_clear

//debug levels
#define OFF       0x00
#define SCHED     0x01
#define LOCK      0x02
#define FRAME     0x04
#define IVAR      0x08
#define REDUCER   0x10
#define PEDIGREE  0x20
#define QUEUE     0x40

#define DBG_MASK 0xff


/**
 * print out the thread and a message when ivar debug is turned on
 */
#ifndef IVAR_DBG
#define IVAR_DBG 0 //off by default
#endif

#ifdef IVAR_DBG_ON
#define cilk_dbg(lvl, ...) if (lvl & IVAR_DBG) {    \
  pthread_t id = pthread_self(); char buf[512];             \
  sprintf(buf, __VA_ARGS__);                                \
  volatile struct __cilkrts_worker*volatile tw = __cilkrts_get_tls_worker(); \
  fprintf(stderr, "[tid/W %3d %2d/%p] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, tw, buf); }
#else
#define cilk_dbg(lvl, ...) 
#endif


__CILKRTS_END_EXTERN_C
#endif

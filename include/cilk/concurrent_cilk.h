#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>

__CILKRTS_BEGIN_EXTERN_C

// Here is the structure of an IVar -- it is a simple pair:
typedef struct {
    volatile uintptr_t  __header; /* Exists in one of two states:
                          - 0: indicates that the ivar is EMPTY (none waiting)
                          - CILK_IVAR_FULL: indicates that the ivar is FULL
                          - other: pointer to waitlist for empty ivar
                         */
    volatile ivar_payload_t __value; /* A pointer to the final contents, when available. */
}  __cilkrts_ivar;

CILK_API(ivar_payload_t) __cilkrts_ivar_read (__cilkrts_ivar*);
CILK_API(void)           __cilkrts_ivar_write(__cilkrts_ivar*, ivar_payload_t);
CILK_API(void)           __cilkrts_ivar_clear(__cilkrts_ivar*);

typedef volatile struct __cilkrts_paused_stack* PAUSED_FIBER;
CILK_API(void) __cilkrts_finalize_pause(struct __cilkrts_worker* w,  PAUSED_FIBER stk);
CILK_API(void) __cilkrts_undo_pause    (struct __cilkrts_worker* w,  PAUSED_FIBER stk);
CILK_API(void) __cilkrts_wake_stack    (PAUSED_FIBER stk);
CILK_API(void) __cilkrts_pause_a_bit   (struct __cilkrts_worker* w);
CILK_API(void) __cilkrts_msleep(unsigned long millis);
CILK_API(void) __cilkrts_usleep(unsigned long micros);
void __cilkrts_show_threadid();
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

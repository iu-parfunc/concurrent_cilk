#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>
#include <pthread.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <time.h>
#include <setjmp.h>

__CILKRTS_BEGIN_EXTERN_C

typedef long __cilkrts_ivar;
typedef long ivar_payload_t;

/**
 * IVar API
 */
typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;
CILK_API(ivar_payload_t) __cilkrts_ivar_read (__cilkrts_ivar*);
CILK_API(void)           __cilkrts_ivar_write(__cilkrts_ivar*, ivar_payload_t);
CILK_API(void)           __cilkrts_ivar_clear(__cilkrts_ivar*);

#define IVAR_SHIFT 0x4
#define IVAR_MASK  0xf
#define CILK_IVAR_EMPTY  0x0
#define CILK_IVAR_FULL   0x1
#define CILK_IVAR_PAUSED 0x2
#define CILK_IVAR_LOCKED 0x4

#define IVAR_EMPTY(iv)  (iv == CILK_IVAR_EMPTY)
#define IVAR_READY(iv)  ((iv & 0xf) == CILK_IVAR_FULL)
#define IVAR_PAUSED(iv) ((iv & 0xf) == CILK_IVAR_PAUSED)
#define TAG(iv)   (iv << IVAR_SHIFT)
#define UNTAG(iv) (iv >> IVAR_SHIFT)

//convenience aliases for a kinder use of ivars in C
typedef __cilkrts_ivar ivar_t;
#define read_iv  __cilkrts_ivar_read
#define write_iv __cilkrts_ivar_write
#define clear_iv __cilkrts_ivar_clear

#ifdef IVAR_DBG

/**
 * print out the thread and a message when ivar debug is turned on
 */
#define ivprintf(lvl, ...) if(IVAR_DBG >= lvl) {    \
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
//--------------------------------------

/** 
 * Concurrent Cilk API
 */
// CSZ: it is necessary that pause be a macro because the longjump must return to a valid frame. 
// you will experience erratic behavior if this is not the case
#define __cilkrts_pause_fiber(ctx)  (setjmp((ctx)))
CILK_API(__cilkrts_worker *) __cilkrts_commit_pause(__cilkrts_worker *w);
CILK_API(void) __cilkrts_resume_fiber(__cilkrts_worker *w);
//--------------------------------------



/** 
 * Concurrent Cilk IO API
 */

/**
 * Analogous to standard posix accept(), except only blocks the Cilk
 * fiber rather than the OS thread.  The first argument is a `sockfd`
 * which must already be bound via bind().
 */
CILK_API(int) cilk_accept(int, struct sockaddr *, socklen_t *);

/**
 * Equivalent to standard posix read(), except only blocks the Cilk
 * fiber rather than the OS thread.
 */
CILK_API(int) cilk_read(int, void*, int);
/**
 * Equivalent to standard posix write(), except only blocks the Cilk
 * fiber rather than the OS thread.
 */
CILK_API(int) cilk_write(int, void*, int);

/**
 * Initialize the event loop for the cilk_IO library.  This will spawn
 * an additional thread, which will persist indefinitely.
 * Returns zero on success.
 */
CILK_API(int) cilk_io_init(void);

/**
 * Terminate the event loop for the cilk_IO library.  This will terminate
 * the additional thread that was spawned by the IO library.
 * Returns zero on success.
 */
CILK_API(void) cilk_io_teardown(void);

CILK_API(void) cilk_sleep(long millis);

__CILKRTS_END_EXTERN_C
#endif

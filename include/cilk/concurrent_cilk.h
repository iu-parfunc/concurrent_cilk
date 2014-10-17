#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <setjmp.h>

__CILKRTS_BEGIN_EXTERN_C

typedef long __cilkrts_ivar;
typedef long ivar_payload_t;

/**
 * IVar API
 */

/** 
 * Clear an IVar.
 *
 * Sets the IVar's value to empty (currently 0).
 */
CILK_API(void) __cilkrts_ivar_clear(__cilkrts_ivar*);

/** 
 * Clear an array of IVars.  This may be more efficient than calling __cilkrts_ivar_clear N times.
 *
 * Sets each IVar's value to empty (currently 0).
 */
CILK_API(void) __cilkrts_ivar_array_clear(__cilkrts_ivar* ptr, int size);

/**
 * Heap allocate an array of empty IVars and return a pointer to it.
 */
CILK_API(__cilkrts_ivar*) __cilkrts_new_ivar_array(int size);

typedef struct __cilkrts_paused_stack __cilkrts_paused_stack;
CILK_API(ivar_payload_t) __cilkrts_ivar_read (__cilkrts_ivar*);
CILK_API(void)           __cilkrts_ivar_write(__cilkrts_ivar*, ivar_payload_t);


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
 * Concurrent Cilk API:
 *
 * The API provides functions which transitions a Cilk thread of execution into the following states:
 *
 * +---- From State -----+---- To State ----+---------------- Transition Functions --------------------+
 * |                     |                  |                                                          |
 * |     unpaused       |  pause snapshot  |                  __cilkrts_pause()                       |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |                  |                                                          |
 * |   pause snapshot    |  pause committed |               __cilkrts_commit_pause()                   |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |  (paused work   |                                                          |
 *                             exposed)     |                                                          |
 * |   pause committed   |  pause committed |    __cilkrts_register_paused_worker_for_stealing()      |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |  (paused work   |                                                          |
 * |                     |      hidden)     |                                                          |
 * |   pause committed   |  pause committed |    __cilkrts_remove_paused_worker_from_stealing()       |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |                  |                                                          |
 * |   unpaused         |    unpaused     |    __cilkrts_remove_paused_worker_from_stealing()       |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |                  |                                                          |
 * |   pause committed   |    unpaused     |             __cilkrts_rollback_pause()                   |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |                  |                                                          |
 * |   pause committed   |    unpaused     |          __cilkrts_run_replacement_fiber()               |
 * |---------------------|------------------|----------------------------------------------------------|
 * |                     |                  |                                                          |
 * |     unpaused       |    unpaused     |               __cilkrts_resume_fiber()                   |
 * +---------------------+------------------+----------------------------------------------------------+
 *
 * Example usage of a pause: 
 *
 *--------------------------------------------------------------------------------
 * jmp_buf ctx; 
 * uintptr_t val = (uintptr_t) __cilkrts_pause_fiber(&ctx); //save a return point for the paused work.
 * if (! val) {
 *    __cilkrts_worker *w = __cilkrts_get_tls_worker_fast(); //gets the current thread local worker.
 *    ...
 *    __cilkrts_worker *replacement = __cilkrts_commit_pause(w, &ctx); //pauses w and returns a replacement.
 *    ...
 *    __cilkrts_register_paused_worker_for_stealing(w);  //paused work dequeue can be accessed by runtime.
 *    ...
 *    __cilkrts_run_replacement_fiber(replacement); //invokes cilk scheduler, user code could be called here.
 * } 
 * //resumed paused computation picks up execution here...
 *--------------------------------------------------------------------------------
 */

/**
 * take a snapshot of the current state of the registers. 
 * This function sets the resume point for a paused computation once it has been paused. 
 *
 * @param ctx A pointer to a jmp_buf struct which will store the execution state. 
 * @return 0 upon pause and nonzero upon restoration. 
 */
CILK_API(int) __cilkrts_pause_fiber(jmp_buf *ctx);

/**
 * Commits the pause by associating the restore point (previously populated by __cilkrts_pause_fiber). 
 * 
 * The commit causes Cilk runtime to forget about the current worker and discover a fresh replacement
 * worker in its place. This function preserves the current team and type of the worker. Additionally,
 * core local state (for concurrent record keeping) is propagated to the replacement. 
 *
 * @param w The worker to pause
 * @param ctx The saved context to use as the restore point for the paused computation upon restoration.
 * @return A new  __cilkrts_worker * set up to be run while the current worker is paused. 
 */
CILK_API(__cilkrts_worker *) __cilkrts_commit_pause(__cilkrts_worker *w, jmp_buf *ctx);

/**
 * Roll back a pause which has been committed (via __cilkrts_commit_pause()). 
 * The result of this function is that the paused worker is reinstated as the current TLS worker
 * and the pointer to the replacement worker is no longer a valid reference. 
 *
 * This function does not return to the restore point but simply continues on with the worker
 * that was the TLS worker before the paused was committed. 
 *
 * The result of calling this function after the Cilk scheduler has been invoked is undefined. 
 *
 * @param paused_w The worker which is currently paused
 * @param replacement_w The worker which replaced paused_w
 */
CILK_API(void) __cilkrts_roll_back_pause(__cilkrts_worker *paused_w, __cilkrts_worker *replacement_w);

/**
 * Register the paused worker's work dequeue to be available for work stealing. 
 *
 * Upon failure of runtime to steal work from the replacement worker, this worker may be 
 * queried and any work available for stealing will be exposed to the thief. 
 *
 * @param w The worker to expose to Cilk runtime for stealing. 
 */
CILK_API(void) __cilkrts_register_paused_worker_for_stealing(__cilkrts_worker *w);

/**
 * Removes a paused worker from being exposed for stealing by Cilk runtime if it exists. 
 * The result is idempotent if the worker is not currently exposed or is a current TLS worker. 
 *
 * @param w The worker to remove from being exposed to Cilk runtime for stealing. 
 */
CILK_API(void) __cilkrts_remove_paused_worker_from_stealing(__cilkrts_worker *w);

/**
 * Invokes the Cilk scheduler to steal work. 
 *
 * @param w The replacement worker which will now become a thief and steal work. 
 */
CILK_API(void) __cilkrts_run_replacement_fiber(__cilkrts_worker *w);

/**
 * Resume a worker who's computation was previously paused. 
 * The restore point as defined by __cilkrts_pause_fiber() will be restored and the
 * worker becomes the current thread local worker. 
 *
 * Conditions under which a restore can be called are as follows: 
 * 1. The calling thread's TLS worker must not have any concurrent work in it's dequeue (w->head == w->tail).
 * 2. The calling thread's TLS worker must not have a next full frame.
 * 3. The w->self value must be equal to the value of the current TLS worker.
 * 4. The worker to be restored must not be the current TLS worker. 
 *
 * @param w The worker which will resume execution at the restore point. 
 * @return Does not return. The paused context of execution is restored. 
 */
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

CILK_API(void) cilk_sleep(long num_micros);

__CILKRTS_END_EXTERN_C
#endif

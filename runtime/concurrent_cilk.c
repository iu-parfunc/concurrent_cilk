// Concurrent Cilk: 
//   An API For pausing/suspending stacks, enabling cooperative multithreading and other forms of
// concurrency.

// API & Usage notes:
// 
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.
//
#include <stdio.h>
#include "scheduler.h"
#include <time.h>
#include "concurrent_cilk_internal.h"
#include "bug.h"
#include "os.h"
#include "os_mutex.h"
#include "local_state.h"
#include "signal_node.h"
#include "full_frame.h"
#include "stacks.h"
#include "sysdep.h"
#include "except.h"
#include "cilk_malloc.h"
#include "pedigrees.h"
#pragma warning(disable: 266)

#define BEGIN_WITH_WORKER_LOCK(w) __cilkrts_worker_lock(w); do
#define END_WITH_WORKER_LOCK(w)   while (__cilkrts_worker_unlock(w), 0)
#define BEGIN_WITH_FRAME_LOCK(w, ff)                                     \
    do { full_frame *_locked_ff = ff; __cilkrts_frame_lock(w, _locked_ff); do

#define END_WITH_FRAME_LOCK(w, ff)                       \
    while (__cilkrts_frame_unlock(w, _locked_ff), 0); } while (0)

CILK_API(void) __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

CILK_API(void) __cilkrts_usleep(unsigned long micros)
{
  struct timespec time;
  time.tv_sec = micros/1000000ul;
  time.tv_nsec = (micros % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

inline CILK_API(void)
__cilkrts_ivar_clear(__cilkrts_ivar* ivar)
{
  *ivar = 0;
}

NOINLINE
static void worker_replacement_scheduler()
{
    __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();
    int stolen = 0;

    // Enter the scheduling loop on the worker. This function will
    // never return
    CILK_ASSERT(w->current_stack_frame);
    w->current_stack_frame->flags |= CILK_FRAME_BLOCKED;

  //  if(w->current_stack_frame->flags & CILK_FRAME_STOLEN)
  //    stolen = 1;

    make_unrunnable(w, w->paused_ff, w->current_stack_frame, 1, "pause");

  //  if(! stolen)
  //    w->current_stack_frame->flags &= ~CILK_FRAME_STOLEN;

    __cilkrts_run_scheduler_with_exceptions((__cilkrts_worker *) w);

    CILK_ASSERT(0);
}

//note: the w passed in here must be the current tls worker!
static inline void 
setup_replacement_stack_and_run(__cilkrts_worker *w)
{
    void *ctx[5]; // Jump buffer for __builtin_setjmp/longjmp.

    //TODO: cache these suckers and get from cache here
    if(! w->l->scheduler_stack)
      w->l->scheduler_stack = sysdep_make_tiny_stack(w);
    CILK_ASSERT(w->l->scheduler_stack);

    // Move the stack pointer onto the scheduler stack.  The subsequent
    // call will move execution onto that stack.  We never return from
    // that call, and every time we longjmp_into_runtime() after this,
    // the w->l->env jump buffer will be populated.
    if (0 == __builtin_setjmp(ctx)) {
        ctx[2] = w->l->scheduler_stack; // replace the stack pointer.
        __builtin_longjmp(ctx, 1);
    } else {
        // We can't just pass the worker through as a parameter to
        // worker_user_scheduler because the generated code might try to
        // retrieve w using stack-relative addressing instead of bp-relative
        // addressing and would get a bogus value.
        worker_replacement_scheduler(); // noinline, does not return.
        CILK_ASSERT(0); // Should never reach this point.
    }
}

//restore the context of a paused worker
inline void
restore_paused_worker(__cilkrts_paused_stack *pstk) 
{
  CILK_ASSERT(pstk);

  __cilkrts_worker *w = pstk->w;
  CILK_ASSERT(w);

  //------------restore context------------
  memcpy(&w->l->env, &pstk->env, sizeof(jmp_buf));
  w->l->frame_ff = pstk->ff;

  //TODO cache these suckers
  //sysdep_destroy_tiny_stack(w->l->scheduler_stack);

  pstk->ff->blocked = 0;
  w->l->scheduler_stack = pstk->scheduler_stack;
  w->l->team = pstk->team;
  w->is_blocked = pstk->is_blocked;
  w->current_stack_frame = pstk->current_stack_frame;
  w->current_stack_frame->flags &= ~CILK_FRAME_BLOCKED;
  w->current_stack_frame->flags |=  CILK_FRAME_BLOCKED_RETURNING;
  //------------end restore context------------
  //
  longjmp(pstk->ctx, 1);
  CILK_ASSERT(0); //does not return
}

// Commit the pause.
CILK_API(void)
__cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack *pstk) 
{
    //-------------save context of the worker-------
    memcpy(&pstk->env, &w->l->env, sizeof(jmp_buf));
    pstk->w = w;
    //pstk->ff = w->l->frame_ff;
    pstk->team = w->l->team;
    pstk->is_blocked = w->is_blocked;
    pstk->scheduler_stack = w->l->scheduler_stack;
    pstk->current_stack_frame = w->current_stack_frame;
    pstk->ff = w->l->frame_ff;
    pstk->ff->blocked = 1;
    w->paused_ff = w->l->frame_ff;
    printf("pausing full frame %p\n", pstk->ff);
    //---------end save context of the worker-------
    
  w->is_blocked = 1;

  BEGIN_WITH_WORKER_LOCK(w) {

    BEGIN_WITH_FRAME_LOCK(w, w->paused_ff) {
      w->l->frame_ff = NULL;
    } END_WITH_FRAME_LOCK(w, w->paused_ff);

  } END_WITH_WORKER_LOCK(w);

  setup_replacement_stack_and_run(w);
  CILK_ASSERT(0); //no return
}


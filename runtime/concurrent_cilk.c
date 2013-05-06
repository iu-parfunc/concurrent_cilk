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
#include "concurrent_cilk_forwarding_array.h"
#include <concurrent_queue.h>

#   define max(a, b) ((a) < (b) ? (b) : (a))

//for nanosleep "declared implicity"...it should be in time.h but isn't?
#pragma warning(disable: 266)

//for atomic_release
#pragma warning(disable: 2206)

#   pragma warning(disable:266)   // disable warning that nanosleep is implicitely defined. it SHOULD be in time.h...
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

inline void
freeze_frame(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf)
{

  //ASSERT: frame lock owned
  
  CILK_ASSERT(ff);
  CILK_ASSERT(sf);

  if(! w->is_blocked) {
    printf("recorded last blocked full frame as %p\n", ff);
    ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED_LAST;
    w->paused_ff  = ff;
  }

  //worker is now marked as blocked
  w->is_blocked = 1;

  /* CALL_STACK becomes valid again */
  ff->call_stack = sf;

  sf->flags |= CILK_FRAME_SUSPENDED | CILK_FRAME_BLOCKED;

  __cilkrts_put_stack(ff, sf);

  //this full frame is now marked as a blocked frame
  ff->concurrent_cilk_flags |= FULL_FRAME_BLOCKED;
}

//restore the context of a paused worker
  inline void
thaw_frame(__cilkrts_paused_stack *pstk) 
{

  __cilkrts_stack_frame *sf;
  full_frame *ff;

  CILK_ASSERT(pstk);
  ff = pstk->ff;
  sf = pstk->current_stack_frame;
  __cilkrts_worker *w = pstk->w;
  CILK_ASSERT(w);

  //------------restore context------------
  w->l->team                    = pstk->team;
  w->l->frame_ff                = pstk->ff;
  w->is_blocked                 = pstk->is_blocked;
  w->l->scheduler_stack         = pstk->scheduler_stack;
  w->current_stack_frame        = pstk->current_stack_frame;
  w->current_stack_frame->flags = pstk->flags;
  memcpy(&w->l->env, &pstk->env, sizeof(jmp_buf));
  //------------end restore context------------

  //fixup the flags
  //the full frame loses its blocked marking
  //and the stack frame loses its blocked marking
  //and gains a flag for a blocked frame that is now returning
  //the full frame is marked as being a self stolen so we know to take
  //the fast path in leave frame.

  sf->flags &= ~CILK_FRAME_BLOCKED;
  sf->flags |=  CILK_FRAME_BLOCKED_RETURNING;

  ff->concurrent_cilk_flags &= ~FULL_FRAME_BLOCKED;
  ff->concurrent_cilk_flags |= FULL_FRAME_SELF_STEAL;

  
  //TODO cache these suckers
  //sysdep_destroy_tiny_stack(w->l->scheduler_stack);
  longjmp(pstk->ctx, 1);
  CILK_ASSERT(0); //does not return
}

// Commit the pause.
  CILK_API(void)
__cilkrts_finalize_pause(__cilkrts_worker* w, __cilkrts_paused_stack *pstk) 
{
  //-------------save context of the worker-------
  pstk->w                   = w;
  pstk->ff                  = w->l->frame_ff;
  pstk->team                = w->l->team;
  pstk->flags               = w->current_stack_frame->flags;
  //pstk->paused_ff           = w->paused_ff;
  pstk->is_blocked          = w->is_blocked;
  pstk->scheduler_stack     = w->l->scheduler_stack;
  pstk->current_stack_frame = w->current_stack_frame;
  memcpy(&pstk->env, &w->l->env, sizeof(jmp_buf));
  //---------end save context of the worker-------

  BEGIN_WITH_WORKER_LOCK(w) {

    BEGIN_WITH_FRAME_LOCK(w, w->l->frame_ff) {
      freeze_frame(w, w->l->frame_ff, w->current_stack_frame);
      w->l->frame_ff = NULL;
    } END_WITH_FRAME_LOCK(w, w->l->frame_ff);

  } END_WITH_WORKER_LOCK(w);

  setup_replacement_stack_and_run(w);
  CILK_ASSERT(0); //no return
}

int paused_stack_trylock(__cilkrts_paused_stack *pstk) {
  return cas(&pstk->lock, 0, 1);
}

void paused_stack_unlock(__cilkrts_paused_stack *pstk) {
  atomic_release(&pstk->lock, 0);
}


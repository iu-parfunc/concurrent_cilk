#ifndef __CONCURRENT_CILK_H
#define __CONCURRENT_CILK_H

#include <cilk/common.h>

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

__CILKRTS_END_EXTERN_C
#endif

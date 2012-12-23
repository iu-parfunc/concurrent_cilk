/*
 *
 * Copyright (C) 2009-2011 , Intel Corporation
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *   * Neither the name of Intel Corporation nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

// NOTE: Presently this code is CONDITIONALLY included/compiled into another file.

// ====================================================================================================

//#include "stack_dbg.h"
#include <stdlib.h>
#define __cilkrts_pause(w)  (CILK_SETJMP((w->current_stack_frame->ctx))) ?  NULL : make_paused_stack((w)) 

struct __cilkrts_ivar_waitlist* make_waitlist_cell() 
{
  struct __cilkrts_ivar_waitlist* newcell = 
    (struct __cilkrts_ivar_waitlist*)
    __cilkrts_malloc(sizeof(struct __cilkrts_ivar_waitlist));;
  // ----------------------------------------
  // <Safety>  We use the special value CILK_IVAR_FULL which should not be in the
  // range of addresses in the heap.  Nevertheless we check here just to be sure:
  if(newcell == (struct __cilkrts_ivar_waitlist*)CILK_IVAR_FULL) {
    newcell = (struct __cilkrts_ivar_waitlist*)__cilkrts_malloc(sizeof(struct __cilkrts_ivar_waitlist));
    __cilkrts_free((void*)CILK_IVAR_FULL);
  }
  // </Safety>
  // ----------------------------------------
  return newcell;
}

   

ivar_payload_t __cilkrts_ivar_read(__cilkrts_ivar* iv)
{

  volatile __cilkrts_ivar* ivar = iv;
  volatile uintptr_t *loc = & ivar->__header; // Is there a better way to do this?

  // First we do a regular load to see if the value is already there.
  // Because it transitions from empty->full only once (and never back) there is no race:
  volatile uintptr_t first_peek = *loc;

  if (first_peek == CILK_IVAR_FULL)
  {
    IVAR_DBG_PRINT_(3," [ivar]   Ivar was already available, returning %lu\n", ivar->__value);
    return ivar->__value;
  }

  // Otherwise we need to do an atomic operation to stick ourselves in the waitlist:
  struct __cilkrts_ivar_waitlist* newcell = make_waitlist_cell();

  // In a valid program by the time we do a read we should be able to use the fast path:
  // In the serial case we'll have already done a write.  In the parallel case a steal must have occurred.
  // In the future maybe external threads will be able to read ivars, but not presently [2011.07.29].
  struct __cilkrts_worker* w = __cilkrts_get_tls_worker_fast();
  volatile struct __cilkrts_paused_stack* ptr = (struct __cilkrts_paused_stack *) __cilkrts_pause(w);

  if (ptr)
  {
    IVAR_DBG_PRINT_(1," [ivar] Creating paused stack: %p\n", ptr);

    newcell->stalled = ptr;
    IVAR_DBG_PRINT_(2," [ivar]  Observed IVar %p in empty state %p, captured continuation in %p.\n", 
        ivar, (void*)first_peek, newcell->stalled);
    // Register the continuation in the front of the waitlist.
    newcell->tail = (struct __cilkrts_ivar_waitlist*)first_peek;    

    while(! __sync_bool_compare_and_swap(loc, first_peek, newcell) ) {             

      IVAR_DBG_PRINT_(1," [ivar]  CAS to extend IVar %p waitlist failed... retrying.\n", ivar);
      // Compare and swap failed; set up to try again:
      first_peek = *loc;

      if (first_peek == CILK_IVAR_FULL) {
        // Well nevermind then... now it is full.
        IVAR_DBG_PRINT_(1," [ivar]  IVar %p became full while we were tring to read it (worker %p), undoing pause on %p.\n", ivar, w, ptr);
        __cilkrts_free(newcell);
        __cilkrts_undo_pause(w,ptr);
        return ivar->__value;
      }
      newcell->tail = (struct __cilkrts_ivar_waitlist*)first_peek;
    }
    IVAR_DBG_PRINT_(1," [ivar]  Added continuation %p to waitlist successfully.  Going to sleep.\n", newcell->stalled);

    __cilkrts_finalize_pause(w,ptr); 
    CILK_ASSERT(0); //should never get here
  }
  // <-- We only jump back to here when the value is ready.
  IVAR_DBG_PRINT_(1," [ivar] returning from setjmp in ivar read\n");
  IVAR_DBG_PRINT_(2," [ivar]  Read of IVar %p: fiber blocked, now awake, returning value %ld.\n", ivar, ivar->__value);
  return ivar->__value;

}

// Wakeup a linked list of stalled compuations.
// This need not be threadsafe because we are the only holder of the list.
void __cilkrts_ivar_wakeup(struct __cilkrts_ivar_waitlist* list) 
{
  IVAR_DBG_PRINT_(1, " [ivar] WAKING UP stalled computation in list entry %p\n", list);

  // The waitlist was atomically grabbed by __cilkrts_ivar_write below, at this point
  // there is no concurrent access to the waitlist.
  while ( list != NULL) {
    __cilkrts_wake_stack( list->stalled );
    list = list->tail;
  }
}

void __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  IVAR_DBG_PRINT_(1,"[ivar] Writing IVar %p, value %lu\n", ivar, val);

  __sync_lock_test_and_set(&ivar->__value, val);

  // Atomically set the ivar to the full state and grab the waitlist:
  ivar_payload_t list = __sync_lock_test_and_set( &ivar->__header, CILK_IVAR_FULL );

  switch((uintptr_t)list) 
  {
    case 0:
      IVAR_DBG_PRINT_(3,"[ivar] looks like no one was waiting on our write...returning\n");
      // It was empty with no one waiting.  Nothing to do.
      break;        
    case CILK_IVAR_FULL:
      // DESIGN DECISION: One could allow multiple puts of the same value.  Not doing so for now.
      __cilkrts_bug("Attempted multiple puts on Cilk IVar in location %p.  Aborting program.\n", ivar);
      break;
    default:
      __cilkrts_ivar_wakeup(list);
  }
}

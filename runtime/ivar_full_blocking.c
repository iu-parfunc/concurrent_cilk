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


#include <stdlib.h>
#include "concurrent_cilk_internal.h"
#include "cilk_malloc.h"
#include <cilk/cilk_api.h>
#include "scheduler.h"
#include "bug.h"

inline
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar* ivar)
{
  __cilkrts_worker *wkr;
  __cilkrts_paused_stack *ptr;
  volatile uintptr_t *header = &ivar->__header; 

  // First we do a regular load to see if the value is already there.
  // Because it transitions from empty->full only once (and never back) there is no race:
  volatile uintptr_t first_peek = *header;

  //fast path -- already got a value
  if(first_peek == CILK_IVAR_FULL) {
    return ivar->__value;
  }

  //slow path -- operation must block until a value is available
  wkr = __cilkrts_get_tls_worker_fast();
  ptr = __cilkrts_pause(wkr);

  if((unsigned long) ptr) {
    // Register the continuation in the ivar's header.
    while(! __sync_bool_compare_and_swap(header, first_peek, ptr) ) {             

      // Compare and swap failed; set up to try again:
      first_peek = *header;

      if(first_peek == CILK_IVAR_FULL) {

        // Well nevermind then... now it is full.
        __cilkrts_undo_pause(wkr,ptr);
        return ivar->__value;
      }
    }

    __cilkrts_finalize_pause(wkr,ptr); 
    CILK_ASSERT(0); //should never get here
  }

  // <-- We only jump back to here when the value is ready.
  return ivar->__value;
}

inline
CILK_API(void) __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  __sync_lock_test_and_set(&ivar->__value, val);

  // Atomically set the ivar to the full state and grab the waitlist:
  volatile __cilkrts_paused_stack *pstk = (volatile __cilkrts_paused_stack *) __sync_headerk_test_and_set( &ivar->__header, CILK_IVAR_FULL );

  switch((uintptr_t)pstk) 
  {
    case 0:
      // It was empty with no one waiting. Nothing to do.
      break;
    case CILK_IVAR_FULL:
      // DESIGN DECISION: One could allow multiple puts of the same value.  Not doing so for now.
      __cilkrts_bug("Attempted multiple puts on Cilk IVar in headeration %p.  Aborting program.\n", ivar);
      break;
    default:
      //__cilkrts_ivar_wakeup(list);
      __cilkrts_wake_stack(pstk);
  }
}


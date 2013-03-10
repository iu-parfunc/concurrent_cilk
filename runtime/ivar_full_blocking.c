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

#define IVAR_SHIFT 0x4
#define IVAR_READY(iv) ((*iv & 0xf) == 1)
#define TAG(iv)   (*iv << IVAR_SHIFT)
#define UNTAG(iv) (*iv >> IVAR_SHIFT)

inline
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar *ivar)
{
  __cilkrts_worker *wkr;
  __cilkrts_paused_stack *ptr;
  __cilkrts_ivar *old_iv;

  //fast path -- already got a value
  if(IVAR_READY(ivar)) {
    return UNTAG(ivar);
  }

  //slow path -- operation must block until a value is available
  wkr = __cilkrts_get_tls_worker_fast();
  ptr = __cilkrts_pause(wkr);

  if((unsigned long) ptr) {

    // Register the continuation in the ivar's header.
    old_iv = (__cilkrts_ivar *) atomic_set(ivar, ((unsigned long) ptr) << IVAR_SHIFT);

    if(IVAR_READY(old_iv)) {

      // Well nevermind then... now it is full.
      __cilkrts_undo_pause(wkr,ptr);
      return UNTAG(old_iv);
    }

    __cilkrts_finalize_pause(wkr,ptr); 
    CILK_ASSERT(0); //should never get here
  }

  // <-- We only jump back to here when the value is ready.
  return UNTAG(ivar);
}

  inline
CILK_API(void) __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t newval = (val << IVAR_SHIFT) | CILK_IVAR_FULL;

  //Atomically set the ivar to the full state and grab the waitlist:
  __cilkrts_ivar *pstk = (__cilkrts_ivar *) atomic_set(ivar, newval);

  //DESIGN DECISION: One could allow multiple puts of the same value.  Not doing so for now.
  if(!pstk) return;
  if_f(IVAR_READY(pstk)) 
      __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);

  __cilkrts_wake_stack((__cilkrts_paused_stack *) UNTAG(pstk));
}

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
CILK_API(ivar_payload_t) __cilkrts_ivar_read(__cilkrts_ivar *ivar)
{
  __cilkrts_worker *w;
  __cilkrts_paused_stack *pstk;
  uintptr_t ptr;
  //fast path -- already got a value
  if(IVAR_READY(*ivar)) {
    return UNTAG(*ivar);
  }

  pstk = __cilkrts_malloc(sizeof(__cilkrts_paused_stack));

  //slow path -- operation must block until a value is available
  w = __cilkrts_get_tls_worker_fast();
  ptr = (uintptr_t) __cilkrts_pause((pstk->ctx));

  if(! ptr) {

    // Register the continuation in the ivar's header.
    do {

      if(IVAR_PAUSED(*ivar)) break;

      if(IVAR_READY(*ivar)) {
        // Well nevermind then... now it is full.
        return UNTAG(*ivar);
      }

    } while(! cas(ivar, 0, (((ivar_payload_t) pstk) << IVAR_SHIFT) | CILK_IVAR_PAUSED));

    __cilkrts_finalize_pause(w, pstk); 
     CILK_ASSERT(0); //no return. heads to scheduler.
  }

  // <-- We only jump back to here when the value is ready.

  printf("ivar returning\n");
  return UNTAG(*ivar);
}

  inline CILK_API(void)
__cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
  //the new value is the actual value with the full ivar tag added to it
  ivar_payload_t newval = (val << IVAR_SHIFT) | CILK_IVAR_FULL;

  //write without checking for any checking. Either this succeeds or there is a bug
  ivar_payload_t old_val = (ivar_payload_t) atomic_set(ivar, newval);

  if(IVAR_PAUSED(old_val)) {
    __cilkrts_paused_stack *pstk = (__cilkrts_paused_stack *) (old_val >> IVAR_SHIFT);
    printf("enqueueing ctx %p\n", pstk->w->ready_queue);
    enqueue(pstk->w->ready_queue, (ELEMENT_TYPE) pstk);
  }

  if(IVAR_READY(old_val)) 
    __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
}

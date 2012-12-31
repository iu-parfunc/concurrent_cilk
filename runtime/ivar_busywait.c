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


#include "concurrent_cilk.h"
#include <cilk/cilk_api.h>
#include "cilk_malloc.h"
#include <stdio.h>

#define IVAR_DBG_PRINT_(lvl, ...) if(IVAR_DBG >= lvl) {    \
   pthread_t id = pthread_self(); char buf[512];             \
   sprintf(buf, __VA_ARGS__);                                \
   struct __cilkrts_worker* tw = __cilkrts_get_tls_worker(); \
   fprintf(stderr, "[tid/W %3d %2d] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, buf); }

ivar_payload_t __cilkrts_ivar_read(__cilkrts_ivar* ivar) 
{
    IVAR_DBG_PRINT_(2," [ivar] Reading IVar %p\n", ivar);

    volatile uintptr_t *loc = & ivar->__header; // Is there a better way to do this?

    // First we do a regular load to see if the value is already there.
    // Because it transitions from empty->full only once (and never back) there is no race:
    uintptr_t first_peek = *loc;

    if (first_peek == CILK_IVAR_FULL)
    {
        IVAR_DBG_PRINT_(2," [ivar]   Ivar was already available, returning %lu\n", ivar->__value);
        return ivar->__value;
    }
    
    if (first_peek == 0 ) {
        IVAR_DBG_PRINT_(1," [ivar]  Observed IVar %p in empty state with no one waiting. BUSY waiting!\n", ivar);
        while( *loc != CILK_IVAR_FULL ) { } // Busy wait.
        IVAR_DBG_PRINT_(1," [ivar] Done busy-waiting on IVar %p, now header has state: %lu, returning value %lu\n", 
                             ivar, ivar->__header, ivar->__value);
        return ivar->__value;
    }
    else
    {
        // In the busy-wait version this will never happen...
        fprintf(stderr," [ivar] ERROR: implementation invariant broken on IVar %p.  Waitlist found in busy-wait version.\n", ivar);
        abort();
    }
}

void __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
    IVAR_DBG_PRINT_(2," [ivar] Writing IVar %p, value %lu\n", ivar, val);
    // Write the value with impunity:
    ivar->__value = val;

    if (ivar->__header == CILK_IVAR_FULL)
        __cilkrts_bug("Attempted multiple puts on Cilk IVar in location %p.  Aborting program.\n", ivar);
    
    // In this busy-wait version we just write it and publish the value to other spinning threads.
    ivar->__header = CILK_IVAR_FULL;
    // Make the modified __header visible:
    __cilkrts_fence(); 
}

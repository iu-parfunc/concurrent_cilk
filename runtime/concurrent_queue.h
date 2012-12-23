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

// Concurrent Queues: a few different options.
//
// Option 1 -- TBB concurrent_queue
// Option 2 -- lock based
// Option 3 -- basic Michael & Scott lockfree queues
//
// This is only here because I was unsure whether it was safe to introduce a dependency on
// libtbb for libcilkrts.so.
//   -Ryan

// ====================================================================================================
// Version 1 : TBB queues
//
#define IVAR_DBG_PRINT_(lvl, ...) if(IVAR_DBG >= lvl) {    \
  pthread_t id = pthread_self(); char buf[512];             \
  sprintf(buf, __VA_ARGS__);                                \
  volatile struct __cilkrts_worker* tw = __cilkrts_get_tls_worker(); \
  fprintf(stderr, "[tid/W %3d %2d/%p] %s", (int)(((int)id)%1000), tw ? tw->self : -999999, tw, buf); }

#ifdef  TBB_QUEUE_VERSION
#include "concurrent_queue_tbb.h"
#endif

// ====================================================================================================
// Versions 2 ,3 & 4: Basic custom queues:

#ifndef LOCKFREE_QUEUE_VERSION
#include <pthread.h>
#endif
#include "concurrent_cilk.h"

#ifdef CACHE_AWARE_QUEUE
#include "queues/cache_aware_queue.h"
#endif 

#ifdef LOCKFREE_QUEUE_VERSION
#include "queues/michael_scott_queue.h"
#endif

#ifdef LOCKING_QUEUE_VERSION
#include "queues/locking_queue.h"
#endif

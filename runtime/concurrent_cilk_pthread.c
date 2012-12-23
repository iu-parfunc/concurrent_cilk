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

// ifdef CILK_IVARS_PTHREAD_VARIANT

// NOTE: Presently this code is CONDITIONALLY included/compiled into another file.

// Concurrent Cilk PTHREAD VERSION: 
// In this PThread based version system threads are used rather than user-level.  But same
// API is exposed.

// API Usage note:
//  The finalize/undo call must return to the same code path as the __cilkrts_pause()==NULL
//  path.  This retains implementation flexibility.

// ================================================================================

#include "cilk/cilk_api.h"
#include "cilk/cilk_undocumented.h"
#include "cilktools/cilkscreen.h"
#include "internal/abi.h"
#include "internal/inspector-abi.h"

#include "global_state.h"
#include "os.h"
#include "os_mutex.h"
#include "bug.h"
#include "local_state.h"
#ifndef _WIN32
// definition of max() for Linux.
#define max(a, b) ((a) < (b) ? (b) : (a))
#endif


extern void __cilkrts_concurrent_yield(__cilkrts_worker *w);

void __cilkrts_concurrent_yield(struct __cilkrts_worker *w)
{
#if __APPLE__ || __MIC__
    sched_yield();
#else
    pthread_yield();
#endif
}


/*
 * __cilkrts_cpu_count
 *
 * Returns the number of CPUs to use.
 */

//csz: this definition exists in os-unix.c
extern int __cilkrts_hardware_cpu_count(void);

int __cilkrts_cpu_count (void)
{
    const char *count = getenv("CILK_NWORKERS");
    int nworkers = 0;

    if (count)
        nworkers = atoi(count);
    if (0 == nworkers)
        nworkers = __cilkrts_hardware_cpu_count();
    if (nworkers < 1)
        nworkers = 1;
    return nworkers;
}



// See documentation in concurrent_cilk.c

enum READY_STATE 
{
    RS_INITIAL = 10,
    RS_PAUSED  = 20,
    RS_WOKEN   = 30
};

// This is slightly different from the function of the same name in concurrent_cilk.c
struct __cilkrts_paused_stack* make_paused_stack(struct __cilkrts_worker* w)
{
    struct __cilkrts_paused_stack* sustk = 
       (struct __cilkrts_paused_stack*)__cilkrts_malloc(sizeof(struct __cilkrts_paused_stack));

    // These fields are not used by the pthread variant:
    sustk->stack = NULL;
    sustk->orig_worker = w;
    sustk->replacement_worker = NULL;

    sustk->ready = RS_INITIAL; // A little three-value protocol here.
    pthread_cond_init(& sustk->cond, NULL);
    pthread_mutex_init(& sustk->mut, NULL);
    return sustk;
}


// Go to sleep if we need to to reach the right number of workeers.
void regulate_worker_population(struct __cilkrts_worker* w) 
{
   int current = w->g->P_current;

   if ( w->l->type == WORKER_USER ) {
       IVAR_DBG_PRINT_(4," [concurrent-cilk] Worker cannot go to sleep to regulate population.  User workers must stay up.\n");
       return;
   }

   // If we used an atomic decrement here then we could overshoot and have too few
   // workers.  Instead we sleep one thread at a time (via CAS) until we hit the exact target number.
   //
   // TODO: Reexamine this.  There may be a lot of contention around P_current.
   while ( current > w->g->P_real ) 
   {
       //IVAR_DBG_PRINT(5) fprintf(stderr," [concurrent-concurrent] Too many workers (%d, desired %d) trying to sleep worker # %d\n", 
       //                          current, w->g->P_real, w->self);
       if ( __sync_bool_compare_and_swap(& w->g->P_current, current, current-1) ) {
           IVAR_DBG_PRINT_(2," [concurrent-cilk]   %2d awake; worker %2d going to sleep until condition %p signaled...\n", 
                           current, w->self, &w->g->restcond);
           pthread_mutex_lock  ( &w->g->restmut);
           pthread_cond_wait   ( &w->g->restcond, &w->g->restmut );
           IVAR_DBG_PRINT_(1," [concurrent-cilk]   Worker %d WOKE UP, received signal...\n", w->self);
           pthread_mutex_unlock( &w->g->restmut);
           break;
       } 
       else current = w->g->P_current;
   }
   // If we fall out of that loop that means the population hit the desired level and we
   // need not sleep this worker.  Or it means we were just woken up via a signal to
   // replenish the population.
}

struct __cilkrts_paused_stack* __cilkrts_pause(struct __cilkrts_worker* w)
{
    struct __cilkrts_paused_stack* stk = make_paused_stack(w); 
    IVAR_DBG_PRINT_(1," [concurrent-cilk] pause(): Creating paused stack: %p\n", stk);
    return stk;
}

void __cilkrts_finalize_pause(struct __cilkrts_worker* w,
                              struct __cilkrts_paused_stack* stk)
{
    IVAR_DBG_PRINT_(1," [concurrent-cilk] finalize_pause on worker %2d, stk %p...\n", w->self, stk);

    // First, wake someone up to replace us, but only if we are under the desired number of awake workers.
    // For example, in __cilkrts_pause_a_bit this will not be neccessary.

    // int current = w->g->P_current;
    // Atomically decrement the global count to discount ourselves:
    int current = __sync_add_and_fetch( & (cilkg_get_global_state())->P_current, -1);

    if ( current < w->g->P_real ) 
    {
        pthread_mutex_lock  (&w->g->restmut);
        pthread_cond_signal (&w->g->restcond);
        pthread_mutex_unlock(&w->g->restmut);
        IVAR_DBG_PRINT_(3," [concurrent-cilk]   Observed %2d awake at pause.  Signaled to wake replacement.\n", current);
    } else
        IVAR_DBG_PRINT_(3," [concurrent-cilk]   Observed %2d awake at pause.  NOT signaling replacement.\n", current);


    pthread_mutex_lock  (&stk->mut);
    if (stk->ready == RS_INITIAL) // If we get here before a wakeup happens...
    {
        stk->ready = RS_PAUSED; // Protected by mutex.
        __sync_synchronize();
        IVAR_DBG_PRINT_(2," [concurrent-cilk]   Pause stk %p, ready %d, got the lock, now pthread_cond_wait on %p\n", 
                        stk, stk->ready, &stk->cond);
        pthread_cond_wait   (&stk->cond, &stk->mut);
        IVAR_DBG_PRINT_(2," [concurrent-cilk]   paused stk %p woke from pthread_cond_wait. Freeing.\n", stk);
        pthread_mutex_unlock(&stk->mut);
    } else {
        IVAR_DBG_PRINT_(1," [concurrent-cilk]   pause of stk %p SKIPPED, already woken.  Freeing.\n", stk);
        pthread_mutex_unlock(&stk->mut);
        // To be nice we make sure to at least pthread yield here:
        // pthread_yield();
        // csz: changed from //__cilkrts_yield(w);
        __cilkrts_yield();
    }
    // Our one wakeup call has completed and this pointer should never be used again.
    __cilkrts_free(stk);
}

void __cilkrts_undo_pause(struct __cilkrts_worker* w,
                          struct __cilkrts_paused_stack* stk)
{
    IVAR_DBG_PRINT_(2," [concurrent-cilk] Pause of %p on worker %d undone.\n", stk, w->self);
    __cilkrts_free(stk);
}

// Must only be called once.  Not idempotent.
void __cilkrts_wake_stack(struct __cilkrts_paused_stack* stk)
{
    IVAR_DBG_PRINT_(1," [concurrent-cilk] wake_stack on stack %p...\n", stk);
    CILK_ASSERT(stk);
    pthread_mutex_lock(&stk->mut);
    IVAR_DBG_PRINT_(2," [concurrent-cilk]   Attempting to wake stk %p -- got the lock.\n", stk);
    // If we are the first one to get here AFTER it was finalized, we signal it:
    if (stk->ready == RS_PAUSED) {
        IVAR_DBG_PRINT_(2," [concurrent-cilk]   Wakeup really-sleeping stack %p, signal condition %p.\n", stk, &stk->cond);
        __sync_synchronize();
        pthread_cond_signal(& stk->cond);

        // Here we do an atomic increment of the number of active workers.  The previous
        // cond_signal necessarily added one to the pool.
        CILK_ASSERT(stk->orig_worker);
        // int awake = __sync_add_and_fetch( & stk->orig_worker->g->P_current, 1);
        int awake = __sync_add_and_fetch( & (cilkg_get_global_state())->P_current, 1);

        IVAR_DBG_PRINT_(2," [concurrent-cilk]   Atomically incremented number of awake workers, now at %2d.\n", awake);
    }
    else IVAR_DBG_PRINT_(3," [concurrent-cilk]   We were not entitled to signal wake of %p (ready %d); doing nothing.\n", 
                         stk, stk->ready);
    stk->ready = RS_WOKEN;
    pthread_mutex_unlock(&stk->mut); 
    
}



// RRN: This alternative function computes an inflated number of
// workers to allow for (initially stalled on a semaphore) spare workers.
int __cilkrts_calculate_P( global_state_t *g)
{
  //csz: changed user_settabel_values_t to global_state_t and removed the paramater to the function.
  //the *user_settable_values now comes from cilkg_get_user_settable_values()
  //it seems like perhaps this could get deleted since all the info is in global state now.
  
  extern global_state_t *cilkg_get_user_settable_values();
  global_state_t *user_settable_values = cilkg_get_user_settable_values();
    if (NULL != g)
        return g->P;

// RRN: We need to allow a large number of workers in this mode:
// #define CILK_IVARS_PTHREAD_HARDCODED_THREADCOUNT 512
#define CILK_IVARS_PTHREAD_HARDCODED_THREADCOUNT 1
// In this version we are IGNORING the users nworkers setting:
    int P_real = __cilkrts_cpu_count();
    int P_inflated = max(CILK_IVARS_PTHREAD_HARDCODED_THREADCOUNT, 4 * P_real);
//    int P_inflated = max(CILK_IVARS_PTHREAD_HARDCODED_THREADCOUNT, P_real);

    // OVERWRITE the value set by __cilkrts_calculate_P_real:
    //csz changed from user_settable_values->nworkers to user_settable_values->P
    user_settable_values->P = P_inflated;

    IVAR_DBG_PRINT_(1," [concurrent-cilk-pthread] CALCULATE_P returning %d\n", P_inflated);
    return P_inflated;
}

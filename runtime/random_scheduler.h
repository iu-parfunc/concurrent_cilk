#ifndef INCLUDED_RANDOM_SCHEDULER_DOT_H
#define INCLUDED_RANDOM_SCHEDULER_DOT_H
__CILKRTS_BEGIN_EXTERN_C

/*************************************************************
   THE protocol:
*************************************************************/

/* the infinity value of E */
#define EXC_INFINITY  ((__cilkrts_stack_frame **) (-1))
void increment_E(__cilkrts_worker *victim);
void decrement_E(__cilkrts_worker *victim);
void reset_THE_exception(__cilkrts_worker *w);

/*************************************************************
             RANDOM SCHEDULER IMPLEMENTATION
*************************************************************/

/*
 * Try to do work.  If there is none available, try to steal some and do it.
 * This is the classic (and default) cilk scheduler
 */
void random_work_steal_sched(__cilkrts_worker *w, void *args);

/* detach the top of the deque frame from the VICTIM and install a new
   CHILD frame in its place */
void detach_for_steal(
    __cilkrts_worker *w, __cilkrts_worker *victim, __cilkrts_stack *sd);


void random_steal(__cilkrts_worker *w);

/* w should be the currently executing worker.  
 * loot_sf is the youngest stack frame in the call stack being 
 *   unrolled (i.e., the most deeply nested stack frame.)
 *
 * When this method is called for a steal, loot_sf should be on a
 * victim worker which is different from w.
 * For CILK_FORCE_REDUCE, the victim worker will equal w.
 *
 * Before execution, the __cilkrts_stack_frame's have pointers from
 * older to younger, i.e., a __cilkrts_stack_frame points to parent.
 *
 * This method creates a full frame for each __cilkrts_stack_frame in
 * the call stack, with each full frame also pointing to its parent. 
 *
 * The method returns the full frame created for loot_sf, i.e., the
 * youngest full frame.
 */
full_frame *unroll_call_stack(
    __cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *const loot_sf);

/* Return true if the frame can be stolen, false otherwise */
int dekker_protocol(__cilkrts_worker *victim);

//--------------------------------------------------------
//                PSEUDO RANDOM NUMBER GENERATOR

unsigned myrand(__cilkrts_worker *w);
void mysrand(__cilkrts_worker *w, unsigned seed);

//--------------------------------------------------------

__CILKRTS_END_EXTERN_C
#endif

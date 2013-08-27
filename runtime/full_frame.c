/* full_frame.c                  -*-C++-*-
 *
 *************************************************************************
 *
 * Copyright (C) 2010-2011 
 * Intel Corporation
 * 
 * This file is part of the Intel Cilk Plus Library.  This library is free
 * software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 * 
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 **************************************************************************/

#include "full_frame.h"
#include "stats.h"
#include "os.h"
#include <bug/bug.h>
#include <jmpbuf/jmpbuf.h>
#include "frame_malloc.h"
#include "global_state.h"
#include "sysdep.h"

#ifdef _WIN32
#   pragma warning(disable:1786)   // disable warning: sprintf is deprecated
#   include "sysdep-win.h"
#endif  // _WIN32

#ifdef CILK_IVARS
#include "internal/abi.h" //for full_frame flags
#endif

COMMON_PORTABLE
full_frame *__cilkrts_make_full_frame(__cilkrts_worker *w,
                                      __cilkrts_stack_frame *sf)
{
    full_frame *ff;

    START_INTERVAL(w, INTERVAL_ALLOC_FULL_FRAME) {
        ff = (full_frame *)__cilkrts_frame_malloc(w, sizeof(*ff));
        __cilkrts_mutex_init(&ff->lock);

        ff->full_frame_magic_0 = FULL_FRAME_MAGIC_0;
        ff->join_counter = 0;
        ff->parent = 0;
        ff->rightmost_child = 0;
        ff->left_sibling = ff->right_sibling = 0;
        ff->call_stack = sf;
        ff->is_call_child = 0;
        ff->simulated_stolen = 0;
	ff->children_reducer_map = ff->right_reducer_map = 0;
        ff->pending_exception = 
            ff->child_pending_exception = 
            ff->right_pending_exception = NULL;

        ff->sync_sp = 0;
#ifdef _WIN32
        ff->exception_sp = 0;
        ff->trylevel = (unsigned long)-1;
        ff->registration = 0;
#endif
	ff->frame_size = 0;
//        ff->exception_sp_offset = 0;
//        ff->eh_kind = EH_NONE;
        ff->stack_self = 0;
        ff->stack_child = 0;

        ff->sync_master = 0;

#ifdef CILK_IVARS
        ff->concurrent_cilk_flags = 0;
#endif

        /*__cilkrts_init_full_frame_sysdep(w, ff);*/
        ff->full_frame_magic_1 = FULL_FRAME_MAGIC_1;
    } STOP_INTERVAL(w, INTERVAL_ALLOC_FULL_FRAME);
    return ff;
}

#ifdef CILK_IVARS
void print_flags(full_frame *ff) 
{
  if(! ff->concurrent_cilk_flags) return;
  printf("<<<<<<<<<<<<<<<<<<< >>>>> full_frame %p has flags ", ff);
  if(ff->concurrent_cilk_flags & FULL_FRAME_BLOCKED)     printf("FULL_FRAME_BLOCKED ");
  if(ff->concurrent_cilk_flags & FULL_FRAME_SELF_STEAL)  printf("FULL_FRAME_SELF_STEAL ");
  printf("\n");
}
#endif

COMMON_PORTABLE void __cilkrts_put_stack(full_frame *ff,
                                         __cilkrts_stack_frame *sf)
{
    /* When suspending frame ff prior to stealing it, __cilkrts_put_stack is
     * used to store the stack pointer for eventual sync.  When suspending
     * frame ff prior to a sync, __cilkrts_put_stack is called to re-establish
     * the sync stack pointer, offsetting it by any change in the stack depth
     * that occured between the spawn and the sync.
     * Although it is not usually meaningful to add two pointers, the value of
     * ff->sync_sp at the time of this call is really an integer, not a
     * pointer.
     */
    ptrdiff_t sync_sp_i = (ptrdiff_t) ff->sync_sp;
    char* sp = (char*) __cilkrts_get_sp(sf);

    ff->sync_sp = sp + sync_sp_i;

    DBGPRINTF("%d-                __cilkrts_put_stack - adjust (+) sync "
              "stack of full frame %p (+sp: %p) to %p\n",
              __cilkrts_get_tls_worker()->self, ff, sp, ff->sync_sp);
}

COMMON_PORTABLE void __cilkrts_take_stack(full_frame *ff, void *sp)
{
    /* When resuming the parent after a steal, __cilkrts_take_stack is used to
     * subtract the new stack pointer from the current stack pointer, storing
     * the offset in ff->sync_sp.  When resuming after a sync,
     * __cilkrts_take_stack is used to subtract the new stack pointer from
     * itself, leaving ff->sync_sp at zero (null).  Although the pointers being
     * subtracted are not part of the same contiguous chunk of memory, the
     * flat memory model allows us to subtract them and get a useable offset.
     */
    ptrdiff_t sync_sp_i = ff->sync_sp - (char*) sp;

    ff->sync_sp = (char *) sync_sp_i;

    DBGPRINTF("%d-                __cilkrts_take_stack - adjust (-) sync "
              "stack of full frame %p to %p (-sp: %p)\n",
              __cilkrts_get_tls_worker()->self, ff, ff->sync_sp, sp);
}

COMMON_PORTABLE
void __cilkrts_destroy_full_frame(__cilkrts_worker *w, full_frame *ff)
{
    validate_full_frame(ff);
    CILK_ASSERT(ff->children_reducer_map == 0);
    CILK_ASSERT(ff->right_reducer_map == 0);
    CILK_ASSERT(NULL == ff->pending_exception);
    CILK_ASSERT(NULL == ff->child_pending_exception);
    CILK_ASSERT(NULL == ff->right_pending_exception);
    __cilkrts_mutex_destroy(w, &ff->lock);
    __cilkrts_frame_free(w, ff, sizeof(*ff));
}

COMMON_PORTABLE void validate_full_frame(full_frame *ff)
{
    /* check the magic numbers, for debugging purposes */
    if (ff->full_frame_magic_0 != FULL_FRAME_MAGIC_0 ||
        ff->full_frame_magic_1 != FULL_FRAME_MAGIC_1)
        abort_because_rts_is_corrupted();
}

void __cilkrts_frame_lock(__cilkrts_worker *w, full_frame *ff)
{
    validate_full_frame(ff);
    __cilkrts_mutex_lock(w, &ff->lock);
}

void __cilkrts_frame_unlock(__cilkrts_worker *w, full_frame *ff)
{
    __cilkrts_mutex_unlock(w, &ff->lock);
}

void double_link(full_frame *left_ff, full_frame *right_ff)
{
    if (left_ff)
        left_ff->right_sibling = right_ff;
    if (right_ff)
        right_ff->left_sibling = left_ff;
}

/* add CHILD to the right of all children of PARENT */
void push_child(full_frame *parent_ff, full_frame *child_ff)
{
    double_link(parent_ff->rightmost_child, child_ff);
    double_link(child_ff, 0);
    parent_ff->rightmost_child = child_ff;
}

/* unlink CHILD from the list of all children of PARENT */
void unlink_child(full_frame *parent_ff, full_frame *child_ff)
{
    double_link(child_ff->left_sibling, child_ff->right_sibling);

    if (!child_ff->right_sibling) {
        /* this is the rightmost child -- update parent link */
        CILK_ASSERT(parent_ff->rightmost_child == child_ff);
        parent_ff->rightmost_child = child_ff->left_sibling;
    }
    child_ff->left_sibling = child_ff->right_sibling = 0; /* paranoia */
}

void incjoin(full_frame *ff)
{
    ++ff->join_counter;
}

int decjoin(full_frame *ff)
{
    CILK_ASSERT(ff->join_counter > 0);
    return (--ff->join_counter);
}

/* Link PARENT and CHILD in the spawn tree */
full_frame *make_child(__cilkrts_worker *w, 
                              full_frame *parent_ff,
                              __cilkrts_stack_frame *child_sf,
                              __cilkrts_stack *sd)
{
    full_frame *child_ff = __cilkrts_make_full_frame(w, child_sf);

    child_ff->parent = parent_ff;
    push_child(parent_ff, child_ff);

    //DBGPRINTF("%d-          make_child - child_frame: %p, parent_frame: %p, child_sf: %p\n"
    //    "            parent - parent: %p, left_sibling: %p, right_sibling: %p, rightmost_child: %p\n"
    //    "            child  - parent: %p, left_sibling: %p, right_sibling: %p, rightmost_child: %p\n",
    //          w->self, child, parent, child_sf,
    //          parent->parent, parent->left_sibling, parent->right_sibling, parent->rightmost_child,
    //          child->parent, child->left_sibling, child->right_sibling, child->rightmost_child);

    CILK_ASSERT(parent_ff->call_stack);
    child_ff->is_call_child = (sd == NULL);

    /* PLACEHOLDER_STACK is used as non-null marker indicating that
       child should be treated as a spawn child even though we have not
       yet assigned a real stack to its parent. */
    if (sd == PLACEHOLDER_STACK)
        sd = NULL; /* Parent actually gets a null stack, for now */

    /* perform any system-dependent actions, such as capturing
       parameter passing information */
    /*__cilkrts_make_child_sysdep(child, parent);*/

    /* Child gets reducer map and stack of parent.
       Parent gets a new map and new stack. */
    child_ff->stack_self = parent_ff->stack_self;
    child_ff->sync_master = NULL;

    if (child_ff->is_call_child) {
        /* Cause segfault on any attempted access.  The parent gets
           the child map and stack when the child completes. */
        parent_ff->stack_self = 0;
    } else {
        parent_ff->stack_self = sd;
        __cilkrts_bind_stack(parent_ff,
                             __cilkrts_stack_to_pointer(parent_ff->stack_self, child_sf),
                             child_ff->stack_self,
                             child_ff->sync_master);
    }

    incjoin(parent_ff);
    return child_ff;
}

/* End full_frame.c */

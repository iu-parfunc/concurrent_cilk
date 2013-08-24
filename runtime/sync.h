#ifndef INCLUDED_SYNC_DOT_H
#define INCLUDED_SYNC_DOT_H

__CILKRTS_BEGIN_EXTERN_C

void set_sync_master(__cilkrts_worker *w, full_frame *ff);
void unset_sync_master(__cilkrts_worker *w, full_frame *ff);
NORETURN __cilkrts_c_sync(__cilkrts_worker *w, __cilkrts_stack_frame *sf_at_sync);
void __cilkrts_mark_synched(full_frame *ff);
void do_sync(__cilkrts_worker *w, full_frame *ff, __cilkrts_stack_frame *sf);

__CILKRTS_END_EXTERN_C
#endif

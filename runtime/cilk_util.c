#include <internal/abi.h>
#include "local_state.h"


void validate_worker(__cilkrts_worker *w)
{
    /* check the magic numbers, for debugging purposes */
    if (w->l->worker_magic_0 != WORKER_MAGIC_0 ||
        w->l->worker_magic_1 != WORKER_MAGIC_1)
        abort_because_rts_is_corrupted();
}

/* conditions under which victim->head can be stolen: */
//static
int can_steal_from(__cilkrts_worker *victim)
{
    return ((victim->head < victim->tail) && 
            (victim->head < victim->protected_tail));
}



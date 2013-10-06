/* worker_mutex.c                  -*-C-*-
 *
 *************************************************************************
 *
 * Copyright (C) 2009-2011 
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
 **************************************************************************/

#include <bug/bug.h>
#include <core/os.h>
#include <core/worker_mutex.h>
#include <core/scheduler.h>

#include <stats/stats.h>
#include <util/cilk_util.h>

/* m->lock == 1 means that mutex M is locked */
#define TRY_ACQUIRE(m) (__cilkrts_xchg(&(m)->lock, 1) == 0)

/* ICC 11.1+ understands release semantics and generates an
   ordinary store with a software memory barrier. */
#if __ICC >= 1110
#define RELEASE(m) __sync_lock_release(&(m)->lock)
#else
#define RELEASE(m) __cilkrts_xchg(&(m)->lock, 0)
#endif

void __cilkrts_mutex_init(struct mutex *m)
{
    m->owner = 0;

    // Use a simple assignment so Inspector doesn't bug us about the
    // interlocked exchange doing a read of an uninitialized variable.
    // By definition there can't be a race when we're initializing the
    // lock...
    m->lock = 0;
}

void __cilkrts_mutex_lock(__cilkrts_worker *w, struct mutex *m)
{
    int count;
    const int maxspin = 1000; /* SWAG */

    NOTE_INTERVAL(w, INTERVAL_MUTEX_LOCK);
    if (!TRY_ACQUIRE(m)) {
	START_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
	count = 0;
	do {
	    do {
		__cilkrts_short_pause();

		if (++count >= maxspin) {
		    STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
		    START_INTERVAL(w, INTERVAL_MUTEX_LOCK_YIELDING);
		    /* let the OS reschedule every once in a while */
		    __cilkrts_yield();
		    STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_YIELDING);
		    START_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
		    count = 0;
		}
	    } while (m->lock != 0);
	} while (!TRY_ACQUIRE(m));
	STOP_INTERVAL(w, INTERVAL_MUTEX_LOCK_SPINNING);
    }

    CILK_ASSERT(m->owner == 0);
    m->owner = w;
}

int __cilkrts_mutex_trylock(__cilkrts_worker *w, struct mutex *m)
{
    NOTE_INTERVAL(w, INTERVAL_MUTEX_TRYLOCK);
    if (TRY_ACQUIRE(m)) {
        CILK_ASSERT(m->owner == 0);
        m->owner = w;
        return 1;
    } else {
        return 0;
    }
}

void __cilkrts_mutex_unlock(__cilkrts_worker *w, struct mutex *m)
{
    CILK_ASSERT(m->owner == w);
    m->owner = 0;
    RELEASE(m);
}

void __cilkrts_mutex_destroy(__cilkrts_worker *w, struct mutex *m)
{
    (void)w; /* unused */
    (void)m; /* unused */
}

/* W grabs its own lock */
void __cilkrts_worker_lock(__cilkrts_worker *w)
{
    validate_worker(w);
    CILK_ASSERT(w->l->do_not_steal == 0);

    /* tell thieves to stay out of the way */
    w->l->do_not_steal = 1;
    __cilkrts_fence(); /* probably redundant */

    __cilkrts_mutex_lock(w, &w->l->lock);
}

void __cilkrts_worker_unlock(__cilkrts_worker *w)
{
    __cilkrts_mutex_unlock(w, &w->l->lock);
    CILK_ASSERT(w->l->do_not_steal == 1);
    /* The fence is probably redundant.  Use a release
       operation when supported (gcc and compatibile);
       that is faster on x86 which serializes normal stores. */
#if defined __GNUC__ && (__GNUC__ * 10 + __GNUC_MINOR__ > 43 || __ICC >= 1110)
    __sync_lock_release(&w->l->do_not_steal);
#else
    w->l->do_not_steal = 0;
    __cilkrts_fence(); /* store-store barrier, redundant on x86 */
#endif
}

/* try to acquire the lock of some *other* worker */
int worker_trylock_other(__cilkrts_worker *w, __cilkrts_worker *other)
{
    int status = 0;

    validate_worker(other);

    /* This protocol guarantees that, after setting the DO_NOT_STEAL
       flag, worker W can enter its critical section after waiting for
       the thief currently in the critical section (if any) and at
       most one other thief.  

       This requirement is overly paranoid, but it should protect us
       against future nonsense from OS implementors.
    */

    /* compete for the right to disturb OTHER */
    if (__cilkrts_mutex_trylock(w, &other->l->steal_lock)) {
        if (other->l->do_not_steal) {
            /* leave it alone */
        } else {
            status = __cilkrts_mutex_trylock(w, &other->l->lock);
        }
        __cilkrts_mutex_unlock(w, &other->l->steal_lock);
    }


    return status;
}

void worker_unlock_other(__cilkrts_worker *w, __cilkrts_worker *other)
{
    __cilkrts_mutex_unlock(w, &other->l->lock);
}

/* End worker_mutex.c */

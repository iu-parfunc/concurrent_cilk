
// NOTE: Presently this code is CONDITIONALLY included/compiled into another file.
// This implementation is only used for DEBUGGING.

// ====================================================================================================

ivar_payload_t __cilkrts_ivar_read(__cilkrts_ivar* ivar) 
{
    dbgprint(IVAR, " [ivar-busywait] Reading IVar %p\n", ivar);
    CILK_ASSERT(ivar);
    while !(IVAR_READY(*ivar)) { }
    return UNTAG(*ivar);
}

void __cilkrts_ivar_write(__cilkrts_ivar* ivar, ivar_payload_t val) 
{
    unsigned short exit = 0;
    ivar_payload_t new_val  = (val << IVAR_SHIFT) | CILK_IVAR_FULL;
    ivar_payload_t old_val; 
    ivar_payload_t volatile peek; 

    dbgprint(IVAR," [ivar-busywait] Writing IVar %p, value %lu\n", ivar, val);
    CILK_ASSERT(ivar);
    do {
      peek = *ivar;
      switch (peek & IVAR_MASK) {
	case CILK_IVAR_PAUSED:
	  __cilkrts_bug(" [ivar-busywait] Found IVar in state CILK_IVAR_PAUSED, which shouldn't happen in busywait version: %p\n", ivar);
	  break;
	case CILK_IVAR_EMPTY:
	  dbgprint(IVAR, "filling empty ivar %p\n", ivar);
	  old_val = casv(ivar, peek, new_val);
	  dbgprint(IVAR, "old_val 0x%lx flags 0x%lx\n", ((uintptr_t) old_val >> IVAR_SHIFT), old_val & IVAR_MASK);
	  if ((*ivar & IVAR_MASK) == CILK_IVAR_FULL)  { exit = 1; }
	  break;  
	case CILK_IVAR_LOCKED:
	  break; //go around again
	case CILK_IVAR_FULL:
	  __cilkrts_bug("Attempted multiple puts on Cilk IVar %p. Aborting program.\n", ivar);
	default:
	  __cilkrts_bug("[write] Cilk IVar %p in corrupted state 0x%x. Aborting program.\n",
	      ivar, *ivar&IVAR_MASK);
      }
    } while(!exit);
}

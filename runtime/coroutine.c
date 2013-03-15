#define apply(FUNC, ...) FUNC(__VA_ARGS__)


__cilkrts_coroutine *new_coroutine(void (*f1), void* f1_args) {
  __cilkrts_coroutine *c  = memalign(64, sizeof(__cilkrts_coroutine));
  __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();

  c->f = f1;
  c->args = f1_args;
  c->slave = get_replacement_worker(w);
  c->slave->self = -2;
  return c;
}

void coroutine_run(__cilkrts_coroutine *c)
{
  __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();
  //the paused stack must live on the stack of the persistent
  //coroutine worker
  __cilkrts_set_tls_worker(c->slave);
  volatile __cilkrts_paused_stack *pstk = __cilkrts_pause(w);

  if(pstk) {
    //housekeeping
    pstk->replacement_worker = c->slave;
    w->pstk = pstk; 
    //self->cont = pstk;

    //publish the slave for stealing.
    inherit_forwarding_array(w, c->slave);

    //call the function 
    c->f(c->args);

    restore_paused_worker(w);
  } else {
    //TODO: cleanup the coroutine! <<< slave workers persist! cleanup!
    CILK_ASSERT(1);
  } 
}

void yieldto(__cilkrts_coroutine *self, __cilkrts_coroutine *ctx)
{
  //save a new contiuation
  volatile __cilkrts_paused_stack *new_cont = __cilkrts_pause(self->slave);

  if(new_cont) {
    new_cont->orig_worker = self->slave->pstk->orig_worker;
    self->slave->pstk = new_cont;
    //yield to ctx!
    if(ctx->slave->pstk)
      restore_paused_worker(ctx->slave);
    else
      coroutine_run(ctx);

  } else {
    //restore self
  }
}

inline void call_cont(__cilkrts_coroutine *self) {
  volatile __cilkrts_paused_stack *pstk = self->slave->pstk;
  self->slave->pstk = NULL;

  //remove the old worker from the forwarding array 
  remove_replacement_worker(self->slave);
  //cache the paused stack for reuse
  enqueue(self->slave->paused_stack_cache, (ELEMENT_TYPE) pstk);


}


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

void __coroutine_run(__cilkrts_coroutine *c)
{
  __cilkrts_worker *w = __cilkrts_get_tls_worker_fast();
  __cilkrts_paused_stack *pstk = __cilkrts_pause(w);
  if(pstk) {
    add_replacement_worker(w,c->slave,pstk); //sets the current worker to be slave!
    //call the function serially
    c->f(c->args);
  } else {
    //TODO: cleanup the coroutine!

  } 
  //the slave restores *w (the original worker)
  restore_paused_worker(c->slave);
}

void yieldto(__cilkrts_coroutine *self, __cilkrts_coroutine *ctx)
{
  //save the old continuation
  __cilkrts_paused_stack *old_cont = self->cont;
  //save a new contiuation
  __cilkrts_paused_stack *new_cont = __cilkrts_pause(self->slave);

  if(new_cont) {
    //yield to ctx!
    if(ctx->slave)
      restore_paused_worker(ctx->slave);
    else
      __coroutine_run(ctx);

  } else {
    //restore self
    self->slave->pstk = old_cont;
  }
}


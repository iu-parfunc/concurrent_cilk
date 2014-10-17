
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <err.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <pthread.h>
#include "concurrent_cilk_internal.h"
#include <cilk/cilk.h>

#include <event2/event.h>
#include <event2/thread.h>

struct rw_data {
  int                fd;
  struct sockaddr *addr;
  socklen_t   *addr_len;
  void*             buf;
  ssize_t           len;
  __cilkrts_ivar     iv;
};

struct event_base *base;

/** Callback functions **/
static void on_accept(evutil_socket_t fd, short flags, void* arg) {

  dbgprint(CILKIO, " [cilkio] In On accept callback..\n");
  struct rw_data* data = (struct rw_data*) arg;
  data->fd = accept(fd, data->addr, data->addr_len);
  if (data->fd > 0) {
    /* Set the client socket to non-blocking mode. */
    if (evutil_make_socket_nonblocking(data->fd) < 0) {
      err(1, "failed to set client socket to non-blocking");
      close(data->fd);
    }
  } else {
    err(1, "Error accepting from client..");
  }

  // Resume worker here
  __cilkrts_ivar_write(&(data->iv), data->fd);
}

static void on_wakeup(evutil_socket_t fd, short flags, void* arg) {

  struct rw_data* data = (struct rw_data*) arg;
  dbgprint(CILKIO, " [cilkio] In On sleep callback..\n");
  /*printf("------ waking up!!!\n");*/
  // Resume worker
  __cilkrts_ivar_write(&(data->iv), data->len);
}

static void on_read(evutil_socket_t fd, short flags, void* arg) {

  dbgprint(CILKIO, " [cilkio] In On read callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  data->len = read(fd, (char*)data->buf, data->len);
  if (data->len < 0)
    err(1, "Error reading from client..");

  // Resume worker 
  __cilkrts_ivar_write(&(data->iv), data->len);
}

static void on_write(evutil_socket_t fd, short flags, void* arg) {

  dbgprint(CILKIO, " [cilkio] In On write callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  data->len = write(fd, (char*)data->buf, data->len);
  if (data->len < 0)
    err(1, "Error writing to client..");

  // Resume  worker here
  dbgprint(CILKIO, " [cilkio] ON WRITE - Writing value %lu to ivar at addresss %p\n", data->len, &(data->iv));
  __cilkrts_ivar_write(&(data->iv), data->len);
}

void* __cilkrts_io_init_helper(void* ignored) {
  dbgprint(CILKIO, " [cilkio] Now on dedicated event-loop thread, begin loop:\n");

  cilk_spawn event_base_loop(base, EVLOOP_NO_EXIT_ON_EMPTY);
  cilk_sync;
  dbgprint(CILKIO, " [cilkio] Exited event loop..\n");
  return NULL;
}

/* Concurrent Cilk I/O public API */

CILK_API(int) cilk_io_init(void) {
  // initialize to use pthread based locking 
  evthread_use_pthreads();

  /* initialize event loop */
  base = event_base_new();

  dbgprint(CILKIO, " [cilkio] event_base_new complete, spawning thread for event loop..\n");

  pthread_t event_thr;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  int rc = pthread_create(&event_thr, &attr, __cilkrts_io_init_helper, 0);  
  return rc;
}

CILK_API(void) cilk_io_teardown(void) {
  /* exits the event loop */
  event_base_loopbreak(base);
  if (base)
      event_base_free(base);
  libevent_global_shutdown();
}

CILK_API(int) cilk_accept(int listen_fd, struct sockaddr *addr, socklen_t *addr_len) {

    /* Set the socket to non-blocking. */
    if (evutil_make_socket_nonblocking(listen_fd) < 0) {
      err(1, "failed to set server socket to non-blocking");
      return -1;
    }

    struct rw_data data;
    __cilkrts_ivar_clear(&data.iv);
    data.addr = addr;
    data.addr_len = addr_len;

    struct event *ev = event_new(base, listen_fd, EV_READ, on_accept, &data);
    event_add(ev, NULL);
    dbgprint (CILKIO, " [cilkio] Adding accept event..\n");

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
    // Block here
    int fd = __cilkrts_ivar_read(&data.iv);
    dbgprint(CILKIO, " [cilkio] CILK_READ Read ivar val %d from ivar at %p\n", fd, &data.iv);

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

    event_free(ev);
    // Returns the result after resuming
    return fd;
}

CILK_API(int) cilk_read(int fd, void *buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data data;
  data.buf = buf;
  data.len = len;
  __cilkrts_ivar_clear(&data.iv);

  struct event *ev = event_new(base, fd, EV_READ, on_read, &data);
  dbgprint (CILKIO, " [cilkio] Adding read event..\n");
  event_add(ev, NULL);

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
  // Pause now
  ssize_t nbytes = __cilkrts_ivar_read(&data.iv);
  dbgprint(CILKIO, " [cilkio] CILK_READ Read ivar val %lu from ivar at %p\n", nbytes, &data.iv);
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

  event_free(ev);
  // Returns the result after resuming
  return nbytes;
}


CILK_API(int) cilk_write(int fd, void* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data data;
  data.buf = buf;
  data.len = len;
  __cilkrts_ivar_clear(&data.iv);

  struct event *ev = event_new(base, fd, EV_WRITE, on_write, &data);
  dbgprint (CILKIO, " [cilkio] Adding write event..\n");
  event_add(ev, NULL);

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
  // Pause now
  ssize_t nbytes = __cilkrts_ivar_read(&data.iv);
  dbgprint(CILKIO, " [cilkio] CILK_WRITE Read ivar val %lu from ivar at %p\n", nbytes, &data.iv);
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

  event_free(ev);
  // Returns the result after resuming
  return nbytes;
}

CILK_API(void) cilk_sleep(long num_microseconds) {
  // We can get rid of this malloc later
  struct rw_data *data = malloc(sizeof(struct rw_data));
  struct event* timeout_ev = NULL;
  struct timeval tv;

  __cilkrts_ivar_clear(&(data->iv));
  tv.tv_sec = 0;
  tv.tv_usec = (time_t)num_microseconds;
  /*tv.tv_sec = (time_t)num_microseconds;*/
  /*tv.tv_usec = 0;*/
  timeout_ev = evtimer_new(base, on_wakeup, data);

  dbgprint (CILKIO, " [cilkio] Adding sleep event..\n");
  /*printf (" [cilkio] Adding sleep event..\n");*/
  evtimer_add(timeout_ev, &tv);
  __cilkrts_ivar_read(&(data->iv));
  event_free(timeout_ev);
}

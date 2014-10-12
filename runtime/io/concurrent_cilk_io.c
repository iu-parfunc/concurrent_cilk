
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <err.h>
#include <pthread.h>
#include "concurrent_cilk_internal.h"
#include <cilk/cilk.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
/* For inet_ntoa. */
#include <arpa/inet.h>

#include <uv.h>

struct rw_data {
  int fd;
  char* buf;
  int len;
  int nbytes;
  __cilkrts_ivar iv;
  int status;
};

static void set_nonblocking(int sock) {
  int r;
  int flags = fcntl(sock, F_GETFL, 0);
  assert(flags >= 0);
  r = fcntl(sock, F_SETFL, flags | O_NONBLOCK);
  assert(r >= 0);
}

/** Callback functions **/
static void on_accept(uv_poll_t* handle, int status, int events) {
  dbgprint(CONCURRENT, " [cilkio] In On accept callback..\n");
  struct rw_data* data = (struct rw_data*) handle->data;
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  int client_fd = accept(data->fd, (struct sockaddr *)&client_addr, &client_len);

  set_nonblocking(client_fd);

  // Resume worker here
  __cilkrts_ivar_write(&(data->iv), client_fd);

  data->fd = client_fd;
}

static void on_read(uv_poll_t* handle, int status, int events) {

  dbgprint(CONCURRENT, " [cilkio] In On read callback..\n");
  struct rw_data* data= (struct rw_data*) handle->data;

  if (data->len > 0) {
    int len = read(data->fd, (char*)data->buf + data->nbytes, data->len);
    if (len < 0) {
      err(1, "Error reading from client..");
      data->status = -1;
      __cilkrts_ivar_write(&(data->iv), data->len);
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len <= 0) {
      // Resume worker 
      __cilkrts_ivar_write(&(data->iv), data->nbytes);
    }
  } 
}

static void on_write(uv_poll_t* handle, int status, int events) {

  dbgprint(CONCURRENT, " [cilkio] In On write callback..\n");
  struct rw_data* data= (struct rw_data*) handle->data;

  while (data->len > 0) {
    int len = write(data->fd, (char*)data->buf + data->nbytes, data->len);

    if (len < 0) {
      err(1, "Error writing to client..");
      data->status = -1;
      __cilkrts_ivar_write(&(data->iv), data->len);
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len <= 0) {
      // Resume  worker here
      dbgprint(CONCURRENT, " [cilkio] ON WRITE - Writing value %d to ivar at addresss %p\n", data->nbytes, &(data->iv));
      __cilkrts_ivar_write(&(data->iv), data->nbytes);
    }
  }
}

void* __cilkrts_io_init_helper(void* ignored) {
  dbgprint(CONCURRENT, " [cilkio] Now on dedicated event-loop thread, begin loop:\n");
  while (1)
      uv_run(uv_default_loop(), UV_RUN_ONCE);
  dbgprint(CONCURRENT, " [cilkio] Exited event loop..\n");
  return NULL;
}

/* Concurrent Cilk I/O public API */

CILK_API(int) cilk_io_init(void) {
  dbgprint(CONCURRENT, " [cilkio] spawning thread for event loop..\n");

  pthread_t event_thr;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  int rc = pthread_create(&event_thr, &attr, __cilkrts_io_init_helper, 0);  
  return rc;
}

CILK_API(void) cilk_io_teardown(void) {
  /* exits the event loop */
  uv_stop(uv_default_loop());
}

CILK_API(int) cilk_accept(int listen_fd) {

    set_nonblocking(listen_fd);

     // Good idea to recycle these allocations
    struct rw_data* data = calloc(1, sizeof(struct rw_data));
    __cilkrts_ivar_clear(&(data->iv));

    data->fd = listen_fd;
    uv_poll_t accept_event;
    uv_poll_init_socket(uv_default_loop(), &accept_event, listen_fd);
    accept_event.data = data;

    dbgprint (CONCURRENT, "Adding accept event ..\n");
    uv_poll_start(&accept_event, UV_READABLE, on_accept);

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
    // Block here
    unsigned long val = __cilkrts_ivar_read(&(data->iv));
    dbgprint(" [cilkio] CILK_READ Read ivar val %lu form ivar at %p\n", val, &(data->iv));

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

    // Returns the result after resuming
    int fd = data->fd;
    uv_poll_stop(&accept_event);
    free(data);

    return fd;
}

CILK_API(int) cilk_read(int fd, void* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(1, sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  data->fd = fd;  
  __cilkrts_ivar_clear(&(data->iv));

  uv_poll_t read_event;
  uv_poll_init_socket(uv_default_loop(), &read_event, fd);
  read_event.data = data;

  dbgprint (CONCURRENT, " [cilkio] Adding read event..\n");
  uv_poll_start(&read_event, UV_READABLE, on_read);

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
  // Pause now
  unsigned long val = __cilkrts_ivar_read(&(data->iv));
  dbgprint(" [cilkio] CILK_READ Read ivar val %lu form ivar at %p\n", val, &(data->iv));
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

  // Returns the result after resuming
  int nbytes = data->nbytes;
  uv_poll_stop(&read_event);
  free(data);

  return nbytes;
}


CILK_API(int) cilk_write(int fd, void* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(1, sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  data->fd = fd;
  __cilkrts_ivar_clear(&(data->iv));   

  uv_poll_t write_event;
  uv_poll_init_socket(uv_default_loop(), &write_event, fd);
  write_event.data = data;

  dbgprint(CONCURRENT, " [cilkio] Adding write event..\n");
  uv_poll_start(&write_event, UV_WRITABLE, on_write);

#if defined(__clang__) // squash a unused variable warning when debug is off. 
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
  // Pause now
  unsigned long val = __cilkrts_ivar_read(&(data->iv));
  dbgprint(" [cilkio] CILK_WRITE Read ivar val %lu form ivar at %p\n", val, &(data->iv));
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

  // Returns the result after resuming
  int nbytes = data->nbytes;
  uv_poll_stop(&write_event);
  free(data);

  return nbytes;
}

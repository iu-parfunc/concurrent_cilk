
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

#include <event2/event.h>

#define SERVER_PORT 5555

struct rw_data {
  int fd;
  char* buf;
  int len;
  int nbytes;
  __cilkrts_ivar iv;
  int status;
  struct event* read_ev;
  struct event* write_ev;
};

int cilk_io_init_enter= 0;
int cilk_io_init_exit = 0;
int cilk_event_base_set = 0;
struct event_base *base;

/*
event_base* cilk_io_init() {

  if (__sync_val_compare_and_swap(&cilk_io_init_enter, 0, 1) == 0) {

    __sync_val_compare_and_swap(&cilk_io_init_exit, 0, 1)

  } else {

    // spin until the event base has been created
    while (__sync_val_compare_and_swap(&cilk_io_init_exit, 1, 1) == 1) ;


  }

}
*/

/** Callback functions **/
void on_accept(evutil_socket_t fd, short flags, void* arg) {

  printf(" [cilkio] In On accept callback..\n");
  struct rw_data* data= (struct rw_data*) arg;
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  int client_fd = accept(fd, (struct sockaddr *)&client_addr, &client_len);

   /* Set the client socket to non-blocking mode. */
  if (evutil_make_socket_nonblocking(client_fd) < 0) {
    err(1, "failed to set client socket to non-blocking");
    close(client_fd);
    return;
   }

  // Resume worker here
  __cilkrts_ivar_write(&(data->iv), client_fd);

  data->fd = client_fd;
  
  /** testing code **/
  // char* buf = malloc(sizeof(char) * 1024);

  // cilk_read(*client_fd, buf, 1024);

}

void on_read(evutil_socket_t fd, short flags, void* arg) {

  printf(" [cilkio] In On read callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  if (data->len > 0) {
    int len = read(fd, (char*)data->buf + data->nbytes, data->len);

    if (len < 0) {
      err(1, "Error reading from client..");
      data->status = -1;
      __cilkrts_ivar_write(&(data->iv), data->len);
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len > 0) {
      event_add(data->read_ev, NULL);
    } else {
      // Resume worker 
      __cilkrts_ivar_write(&(data->iv), data->nbytes);
    }
  } 
  // Resume worker here
  /** testing code **/
  // cilk_write(fd, data->buf, data->len);
}

void on_write(evutil_socket_t fd, short flags, void* arg) {

  printf(" [cilkio] In On write callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  while (data->len > 0) {
    int len = write(fd, (char*)data->buf + data->nbytes, data->len);

    if (len < 0) {
      err(1, "Error writing to client..");
      data->status = -1;
      __cilkrts_ivar_write(&(data->iv), data->len);
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len > 0) {
      event_add(data->write_ev, NULL);
    } else {
      // Resume  worker here
      printf(" [cilkio] ON WRITE - Writing to ivar at address %p\n", &(data->iv));
      printf(" [cilkio] ON WRITE - Writing value %d to ivar\n", data->nbytes);
      fflush(0);
      __cilkrts_ivar_write(&(data->iv), data->nbytes);
    }
  }
}

void* __cilkrts_io_init_helper(void* ignored) {
  printf(" [cilkio] Now on dedicated event-loop thread, begin loop:\n");
  event_base_loop(base, EVLOOP_NO_EXIT_ON_EMPTY); 
  printf(" [cilkio] Exited event loop..\n");
  return NULL;
}

/* Concurrent Cilk I/O public API */

int cilk_io_init() {
  /* initialize event loop */
  base = event_base_new();

  printf(" [cilkio] event_base_new complete, spawning thread for event loop..\n");

  pthread_t event_thr;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  int rc = pthread_create(&event_thr, &attr, __cilkrts_io_init_helper, 0);  
  return rc;
}

void cilk_io_teardown() {

  /* exits the event loop */
  event_base_loopbreak(base);

}

CILK_API(int) cilk_accept(int listen_fd) {

    /* Set the socket to non-blocking. */
    if (evutil_make_socket_nonblocking(listen_fd) < 0) {
      err(1, "failed to set server socket to non-blocking");
      return -1;
    }

     // Good idea to recycle these allocations
    struct rw_data* data = calloc(sizeof(struct rw_data),1);
    __cilkrts_ivar_clear(&(data->iv));   

    // Need to pass a struct with worker and client fd included
    struct event *accept_event = event_new(base, listen_fd, EV_READ, on_accept, data);


    printf ("Adding accept event ..\n");
    event_add(accept_event, NULL);
    printf ("After adding accept event ..\n");

    // Block here
    __cilkrts_ivar_read(&(data->iv));

    // Returns the result after resuming
    int fd = data->fd;
    event_free(accept_event);
    free(data);

    return fd;

}

CILK_API(int) cilk_read(int fd, void* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(sizeof(struct rw_data), 1);
  data->buf = buf;
  data->len = len;
  __cilkrts_ivar_clear(&(data->iv));   

  struct event* read_event = event_new(base, fd, EV_READ, on_read, data);
  data->read_ev= read_event;
  printf ("Adding read event..\n");
  event_add(read_event, NULL);

  // Pause now
  __cilkrts_ivar_read(&(data->iv));

  // Returns the result after resuming
  int nbytes = data->nbytes;
  event_free(read_event);
  free(data);

  return nbytes;

}

CILK_API(int) cilk_write(int fd, void* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(sizeof(struct rw_data), 1);
  data->buf = buf;
  data->len = len;
  __cilkrts_ivar_clear(&(data->iv));   

  struct event* write_event= event_new(base, fd, EV_WRITE, on_write, data);
  data->write_ev = write_event;
  printf(" [cilkio] Adding write event..\n");
  printf(" [cilkio] CILK_WRITE ivar address %p\n", &(data->iv));
  event_add(write_event, NULL);

  // Pause now
  printf(" [cilkio] CILK_WRITE Write ivar %p\n", &(data->iv));
  // __cilkrts_ivar_write(&(data->iv), 3423);
  unsigned long val = __cilkrts_ivar_read(&(data->iv));
  printf(" [cilkio] CILK_WRITE Read ivar val : %lu\n", val);

  // Returns the result after resuming
  int nbytes = data->nbytes;
  event_free(write_event);
  free(data);

  return nbytes;

}

/*
int mainv(int argc, char** argv) {

  int listen_fd;
  struct sockaddr_in listen_addr;
  int reuseaddr_on = 1;

  listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_fd < 0)
    err(1, "listen failed");
  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr_on,
        sizeof(reuseaddr_on)) == -1)
    err(1, "setsockopt failed");

  memset(&listen_addr, 0, sizeof(listen_addr));
  listen_addr.sin_family = AF_INET;
  listen_addr.sin_addr.s_addr = INADDR_ANY;
  listen_addr.sin_port = htons(SERVER_PORT);

  if (bind(listen_fd, (struct sockaddr *)&listen_addr, sizeof(listen_addr)) < 0) {
    err(1, "bind failed");
  }

  if (listen(listen_fd, 5) < 0) {
    err(1, "listen failed");
  }

  pthread_t event_thr;
  pthread_attr_t attr;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  long t;
  int rc = pthread_create(&event_thr, &attr, cilk_io_init, (void *)t);
  if (rc){
    printf(" [cilkio] ERROR; Failed to create event loop thread with error %d\n", rc);
    exit(-1);
  }

  // Sleep for while until event loop is initialized
  sleep(1);

  printf(" [cilkio] Calling cilk_accept..\n");
  char buf[1025];
  char recvbuf[1025];

  while (1) {
    int fd = cilk_accept(listen_fd);
    strcpy(buf, "Hello client!!!");
    cilk_write(fd, buf, strlen(buf)); 

    int len = cilk_read(fd, recvbuf, 4);
    cilk_write(fd, recvbuf, 4);
  }

  void *status;
  pthread_attr_destroy(&attr);
  rc = pthread_join(event_thr, &status);
  if (rc) {
    printf(" [cilkio] ERROR; return code from pthread_join() is %d\n", rc);
    exit(-1);
  }

  printf(" [cilkio] Exiting server..\n");

}

*/

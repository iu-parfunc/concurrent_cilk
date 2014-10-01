
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <err.h>
#include <pthread.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
/* For inet_ntoa. */
#include <arpa/inet.h>

#include <event2/event.h>

#define SERVER_PORT 5555

struct rw_data {
  char* buf;
  int len;
  int nbytes;
};

int cilk_io_init_enter= 0;
int cilk_io_init_exit = 0;
int cilk_event_base_set = 0;
struct event_base *base;

/**
 *  Set a socket to non-blocking mode.
 * */
int setnonblock(int fd) {
  int flags;
  flags = fcntl(fd, F_GETFL);
  if (flags < 0)
    return flags;
  flags |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, flags) < 0)
    return -1;
  return 0;
}

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

  int* client_fd = (int*) arg;
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  *client_fd = accept(fd, (struct sockaddr *)&client_addr, &client_len);

  // Resume worker here
  
  /** testing code **/
  char* buf = malloc(sizeof(char) * 1024);
  cilk_read(fd, buf, 1024);

}

void on_read(evutil_socket_t fd, short flags, void* arg) {

  struct rw_data* data= (struct rw_data*) arg;
  int len = read(fd, data->buf, data->len);

  data->nbytes = len;

  // Resume worker here

  /** testing code **/
  cilk_write(fd, data->buf, data->len);
}

void on_write(evutil_socket_t fd, short flags, void* arg) {

  struct rw_data* data= (struct rw_data*) arg;
  int len = write(fd, data->buf, data->len);

  data->nbytes = len;

  // Resume worker here
  
}


/* Concurrent Cilk I/O public API */

void* cilk_io_init(void* args) {

  /* initialize event loop */
  base = event_base_new();
  event_base_loop(base, EVLOOP_NO_EXIT_ON_EMPTY);

}

void cilk_io_teardown() {

  /* exits the event loop */
  event_base_loopbreak(base);

}

int cilk_accept(int listen_fd) {

  /*
    if (bind(listen_fd, (struct sockaddr *)&listen_addr,
          sizeof(listen_addr)) < 0)
      err(1, "bind failed");
    if (listen(listen_fd, 5) < 0)
      err(1, "listen failed");
      */

    /* Set the socket to non-blocking. */
    if (setnonblock(listen_fd) < 0)
      err(1, "failed to set server socket to non-blocking");

    int* client_fd = malloc(sizeof(int)); // Make malloc non blocking???
    // Need to pass a struct with worker and client fd included
    struct event *accept_event = event_new(base, listen_fd, EV_TIMEOUT|EV_READ, on_accept, client_fd);
    event_add(accept_event);

    // Pause now
    // free client_fd and and return *client_fd

}

int cilk_read(int fd, char* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = malloc(sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  struct event* read_event = event_new(base, fd, EV_TIMEOUT|EV_READ, on_read, data);
  event_add(read_event);

  // Pause now
  // free rw_data* and return data->nbytes

}

int cilk_write(int fd, char* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = malloc(sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  struct event* read_event = event_new(base, fd, EV_TIMEOUT|EV_WRITE, on_write, data);
  event_add(read_event);

  // Pause now
  // free rw_data* and return data->nbytes

}


int main(int argc, char** argv) {

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
  long t;
  int rc = pthread_create(event_thr, NULL, cilk_io_init, (void *)t);
  if (rc){
    printf("ERROR; Failed to create event loop thread with error %d\n", rc);
    exit(-1);
  }

  // Sleep for while until event loop is initialized
  sleep(1);

  cilk_accept(listen_fd);

}

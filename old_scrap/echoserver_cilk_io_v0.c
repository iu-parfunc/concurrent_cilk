
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
  struct event* read_ev;
  struct event* write_ev;
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

  printf("In On accept callback..\n");
  int* client_fd = (int*) arg;
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  *client_fd = accept(fd, (struct sockaddr *)&client_addr, &client_len);

   /* Set the client socket to non-blocking mode. */
  if (evutil_make_socket_nonblocking(*client_fd) < 0) {
    err(1, "failed to set client socket to non-blocking");
    close(*client_fd);
    return;
   }

  // Resume worker here
  
  /** testing code **/
  char* buf = malloc(sizeof(char) * 10);

  cilk_read(*client_fd, buf, 10);

}

void on_read(evutil_socket_t fd, short flags, void* arg) {

  printf("In On read callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  if (data->len > 0) {
    printf("Going an iteration..\n");
    printf("data->buf : %p\n", data->buf);
    printf("data->nbytes : %d\n", data->nbytes);
    printf("data->len : %d\n", data->len);
    int len = read(fd, data->buf+data->nbytes, data->len);

    if (len < 0) {
      err(1, "Error reading from client..");
      return;
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len > 0) {
      event_add(data->read_ev, NULL);
    } else {
      data->len = data->nbytes;
      data->nbytes = 0;
      cilk_write(fd, data->buf, data->len);
    }
  } 

  // int len = read(fd, data->buf, data->len);

  // data->nbytes = len;

  // Resume worker here

  /** testing code **/
}

void on_write(evutil_socket_t fd, short flags, void* arg) {

  printf("In On write callback..\n");
  struct rw_data* data= (struct rw_data*) arg;

  if (data->len > 0) {
    int len = write(fd, data->buf+data->nbytes, data->len);

    if (len < 0) {
      err(1, "Error writing to client..\n");
      return;
    }

    data->nbytes += len;
    data->len -= len;

    if (data->len > 0) {
      event_add(data->write_ev, NULL);
    }
  }

  // Resume worker here
  
}

/* Concurrent Cilk I/O public API */

void* cilk_io_init(void* args) {

  /* initialize event loop */
  base = event_base_new();
  unsigned char flag = EVLOOP_NONBLOCK;

  printf("Entering event loop..\n");

  event_base_loop(base, EVLOOP_NO_EXIT_ON_EMPTY); 

  printf("Exited event loop..\n");
}

void cilk_io_teardown() {

  /* exits the event loop */
  event_base_loopbreak(base);

}

int cilk_accept(int listen_fd) {

    /* Set the socket to non-blocking. */
    if (evutil_make_socket_nonblocking(listen_fd) < 0) {
      err(1, "failed to set server socket to non-blocking");
      return;
    }

    int* client_fd = calloc(1, sizeof(int)); // Make malloc non blocking???
    
    // Need to pass a struct with worker and client fd included
    struct event *accept_event = event_new(base, listen_fd, EV_READ, on_accept, client_fd);

    printf ("Adding accept event ..\n");
    event_add(accept_event, NULL);
    printf ("After adding accept event ..\n");

    // Pause now
    // free client_fd and and return *client_fd

}

int cilk_read(int fd, char* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(1, sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  struct event* read_event = event_new(base, fd, EV_READ | EV_ET, on_read, data);
  data->read_ev =  read_event;
  printf ("Adding read event..\n");
  event_add(read_event, NULL);

  // Pause now
  // free rw_data* and return data->nbytes

}

int cilk_write(int fd, char* buf, int len) {

  // Good idea to recycle these allocations
  struct rw_data* data = calloc(1, sizeof(struct rw_data));
  data->buf = buf;
  data->len = len;
  struct event* write_event= event_new(base, fd, EV_WRITE | EV_ET, on_write, data);
  printf("Adding write event..\n");
  data->write_ev = write_event;
  event_add(write_event, NULL);

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
  pthread_attr_t attr;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  long t;
  int rc = pthread_create(&event_thr, &attr, cilk_io_init, (void *)t);
  if (rc){
    printf("ERROR; Failed to create event loop thread with error %d\n", rc);
    exit(-1);
  }

  // Sleep for while until event loop is initialized
  sleep(1);

  printf("Calling cilk_accept..\n");
  cilk_accept(listen_fd);

  void *status;
  pthread_attr_destroy(&attr);
  rc = pthread_join(event_thr, &status);
  if (rc) {
    printf("ERROR; return code from pthread_join() is %d\n", rc);
    exit(-1);
  }

  printf("Exiting server..\n");

}

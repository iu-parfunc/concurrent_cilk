/* TO RUN:
 * open a new terminal
 * run ./server_test.exe
 * in the other terminal run: ./server_driver.exe
 */



// UNFINISHED -- the conversion of this test to Ivars is NOT complete.

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <pthread.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <netinet/in.h>
// #include <sys/epoll.h>

// Let's use libevent (or we could use libev or epoll/kqueue directly)
#include <event.h>
#include <evhttp.h>

// #include "../../include/cilk/runtimeconcurrent_queue.h"
#include <cilk/abi.h>
//=========== setting up globals =============
static const int port = 5002;
static const int units = 5;
static const int iters = 10 * 1000 * 1000;
static const int client_limit = 4;

volatile int keep_going = 1;
volatile int total_served = 0;

int do_reads = 0;

const char* response = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n";

struct event_base* global_base;

#define QUEUE_ELEMTY __cilkrts_ivar

int numthreads = 10; // or 30 or 40 ...


void init_ivars(__cilkrts_stack_queue *q) {

  int i;

  for(i = 0; i <= numthreads; i++){
    QUEUE_ELEMTY *iv;
    __cilkrts_ivar_clear(&iv);

      enqueue_paused_stack(q,iv);
  }
}

int read_iv (__cilkrts_ivar *iv) { // types probably wrong here
return __cilkrts_ivar_read(&iv);
}

void write_iv (__cilkrts_ivar *iv, struct val) { // obj type wrong right now

  __cilkrts_ivar_write(iv, val); // do we need to cast val?
}


void *get_in_addr (struct sockaddr *sa) {

  if (sa->sa_family = AF_INET) {
  return &(((struct sockaddr_in*)sa) ->sin6_addr);
  }
  return &(((struct sockaddr_in6*)sa) ->sin6_addr);
}

int main(void) {

  printf("=========== Ivar_server test ==============\n");

  // set up queue
  __cilkrts_stack_queue *q;
  q = make_stack_queue();
  *q = init_ivars(*q);
  // accept loop
  while(1) {
    QUEUE_ELEMTY *iv = dequeue_paused_stack(q);
    if(iv)

  
  
  }

  // found one. Now do something with it
  


  // release him back into the wild












}









/*


//============= basic use functions ===========================

// representing fd's with Ivars. Might want to also considering repping events with
// Iv's as well
void wake_cilk_thread(int fd,short event, void *arg) // might want to be __cilkrts_ivar
{
    printf("wake_cilk_thread callback called with fd: %d, event: %d, arg: %p\n",
            fd, event, arg);
    __cilkrts_wake_stack( (struct __cilkrts_paused_stack*)arg );
}


// dont have anything bloking. So we dont need to create any Ivars.
void cilk_read(int fd, char* buf, int len) 
{
    int i = 0, count = len;
    struct __cilkrts_worker* w = __cilkrts_get_tls_worker_fast();
    // Non-blocking read:
    while (count) 
    {
        int r;
        r = read(fd, buf+i, count);
        if (r<0) { printf("Read error, descriptor %d, errno: %d", fd, errno); abort(); }
        count -= r;
        i += r;
        
        if (count > 0) 
        {
           struct __cilkrts_paused_stack* ptr = __cilkrts_pause(w);
           if (ptr)    
           {
               // Register event handler to wake the thread:
               struct event ev;
               printf("[cilkserv] Setting event callback for cilk_read, event %p\n", &ev);
               event_set(&ev, fd, EV_READ, wake_cilk_thread, &ev);
               __cilkrts_finalize_pause(w,ptr); 
           }
           // When we wake back up we know some data is available so we try to read again.
        } else break;
    }
}
// duplicate of cilk_read
void cilk_write(int fd, char* buf, int len) 
{
    int i = 0, count = len;
    struct __cilkrts_worker* w = __cilkrts_get_tls_worker_fast();
    while (count) 
    {
        int r;
        r = write(fd, buf+i, count);
        if (r<0) { printf("Write error, descriptor %d, errno: %d", fd, errno); abort(); }
        count -= r;
        i += r;
        printf("wrote %d bytes\n", r);
        if (count > 0) 
        {
           struct __cilkrts_paused_stack* ptr = __cilkrts_pause(w);
           if (ptr)    
           {
               // Register event handler to wake the thread:
               struct event ev;
               printf("[cilkserv] Setting event callback for cilk_write, event %p\n", &ev);
               event_set(&ev, fd, EV_WRITE, wake_cilk_thread, &ev);
               __cilkrts_finalize_pause(w,ptr); 
           }
           // When we wake back up we know some data is available so we try to read again.
        } else break;
    }
    printf("Done writing all bytes\n", fd, errno);
    
}

// Handle a client.  Ver1: sequential work chunks.
// Performs IO with client between chunks.
void handle0(int client) 
{
    char buffer[256];
    int total = __sync_add_and_fetch( &total_served, 1);

    if (total >= client_limit) keep_going = 0;
    if (total > client_limit) {
       printf("  __skipped client %d total already %d__\n", client, total);
       // We have done the required amount. END the event loop:
       event_base_loopbreak(global_base);
       return;
    } else printf("   Handling client %d, total served = %d\n", client, total);

    // Do some imaginary work
    // ----------------------------------------
    // We create a random amount of work for each client.
    int quantity = rand() % (iters * units);

    double result = 1.1;
    int i, j;
    for(j=1; j <= units; j++) 
    {
       for(i=0; i < iters; i++) {
           result -= 0.000000001;
           result *= 1.000000001;
       }

       // First draft: just pause a bit.
       // The second step will be to have the client's computation block on IO so that this worker
       // can get some other work done.

       printf("    Finished unit %d for client %d: %lf%s.\n", j, client, result,
              // ", READING byte from socket"
              ""
              );
       struct __cilkrts_worker* w = __cilkrts_get_tls_worker(); // fast?

       // TEMP using blocking OS routines for now:
       // TODO: USE cilk_read above!

       if (do_reads) {
           // cilk_read(client, buffer, 1);
           // printf("    READ byte: %d.\n", (int)buffer[0]);

           buffer[0] = 26;
           cilk_write(client, buffer, 1);
           printf("    WROTE byte: %d.\n", (int)buffer[0]);
       }

       __cilkrts_pause_a_bit(w);
       printf("    Client %d: Back from yielding...\n", client, result);
    }
    // ----------------------------------------
    // TODO: Make a response.

    printf("    Done handling client %d %lf\n", client, result);
    
}



// This version has the problem that the accept loop is not guaranteed to be
// live.  
void run_server(int num, int sockfd) {
    struct sockaddr cli_addr;
    int clientfd;
    socklen_t size = sizeof(cli_addr);
    do {
       struct __cilkrts_worker* w = __cilkrts_get_tls_worker(); // fast?
       printf(" * Running server number %d on worker %d\n", num, w->self);

       clientfd = accept(sockfd, &cli_addr, &size);
       if (clientfd < 0) {
           printf("ERROR: error code from accept(): %d\n", errno);
           abort();
       }
       printf(" ! server number %d GOT CLIENT CONNECTION %d\n", num, clientfd);

       cilk_spawn handle0(clientfd);
       // usleep(100 * 1000);
       w = __cilkrts_get_tls_worker();
       printf(" *=> Server number %d, done or spawned client, now on worker %d\n", num, w->self);
    } while (keep_going); // TODO: Set global stop conditions.
}

void print_usage(char* name) {
    printf("Usage: %s mode P \n", name);
    printf("   Mode is one of: \n");
    printf("     noop - simply accept socket connections at max rate\n");
    printf("     pong - one immediate response on each connection\n");
    exit(-1);
}

void process_request(struct evhttp_request *req, void *arg)
{
    struct evbuffer *buf = evbuffer_new();
    if (buf == NULL) return;

    printf("."); fflush(stdout);

    // TODO: Lets be careful and keep the libevent stuff on one thread for now:
    cilk_spawn handle0(0);
    // tbb::concurrent_queue  queue.enqueue();

    printf("_"); fflush(stdout);

    evbuffer_add_printf(buf, "Requested: %s\n", evhttp_request_uri(req));
    evhttp_send_reply(req, HTTP_OK, "OK", buf);
}


int main(int argc, char **argv) 
{
  // begin unimportant  
  printf("==== server_test: benchmark concurrent event handling...\n");
    if (argc > 3) print_usage(argv[0]);

    char* mode;
    if (argc == 1) mode = "noop";
    else           mode = argv[1];
    
    if ( !strcmp(mode,"noop") ) {
        printf("Noop mode\n");
    } else if( !strcmp(mode,"pong") ) {
        printf("Pong mode\n");
    } else print_usage(argv[0]);

    int P = 8;
    if (argc > 2) P = atoi(argv[2]);
//end unimportant

    struct evhttp *httpd = NULL; // IV1:create
    global_base = event_init(); //IV1:init. Write with some val here to tell us it was an init
    printf(" *** Event system initialized!\n");

    if (global_base == NULL) { // make sure IV was inited properly
      printf("INIT FAILED!\n"); return -1; 
    }
    httpd = evhttp_new(global_base);
    if (httpd == NULL) {
      printf("evhttp_new failed\n"); return -1; 
    }
    if (evhttp_bind_socket(httpd, "127.0.0.1", port) != 0) {
      printf("evhttp_bind_socket failed!\n"); return -1; 
    }
    evhttp_set_gencb(httpd, process_request, NULL);
    printf(" *** Going into event loop...\n");
    event_base_dispatch(global_base);
    printf(" *** OUT OF EVENT LOOP\n");    

    return 0;
}

*/

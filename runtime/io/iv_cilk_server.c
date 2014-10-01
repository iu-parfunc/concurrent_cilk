
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

static const int port = 5002;
static const int units = 5;
static const int iters = 10 * 1000 * 1000;
static const int client_limit = 4;

volatile int keep_going = 1;
volatile int total_served = 0;

int do_reads = 0;

const char* response = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n";

struct event_base* global_base;

//====================================================================================================
// First a mini-library for faux-synchronous IO:

// This is the event handler.  It runs on the main thread (with the event handling loop).
void wake_cilk_thread(int fd, short event, void *arg)
{
    printf("wake_cilk_thread callback called with fd: %d, event: %d, arg: %p\n",
            fd, event, arg);
    __cilkrts_wake_stack( (struct __cilkrts_paused_stack*)arg );
}


// Look! This is so easy to write in a threaded-style rather than event-style:
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
               
               // WARNING: Are we allowed to do this from a different thread?  How about in libevent2?
               event_set(&ev, fd, EV_READ, wake_cilk_thread, &ev);
               // FIXME -- IS THIS A DATA RACE!??  What about events that happen BEFORE this handler goes through?
               // Will it catch already past (buffered?) events?

               __cilkrts_finalize_pause(w,ptr); 
           }
           // When we wake back up we know some data is available so we try to read again.
        } else break;
    }
}

// DUPLICATE CODE (From the above.)
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



//====================================================================================================

// The client workloads.

// The goal here is to test both different [parallel] workload distributions (small/big
// mixes) as well as communication patterns.  In the simple case we take a request from
// the user and communicate only one message at the end.  The more interesting case for
// user threading is multiple rounds of client/server communication in a job.

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

// Version 2: a parfib workload:
void handle1(int client)  {
    // ...
    // Performs IO operations w/client down one spine of the binary tree.

    // TODO - Write me.
}

//====================================================================================================
// Trying a server (accept loop) directly in Cilk:

// We would LIKE consider the following -- make each steal handle one client.
// But this will create arbitrarily deep spawns!
void run_server_WRONG(int sockfd) 
{
    struct sockaddr cli_addr;
    socklen_t size = sizeof(cli_addr);
    int clientfd = accept(sockfd, &cli_addr, &size);
    cilk_spawn run_server_WRONG(sockfd);
    handle0(clientfd);
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

// EXPERIMENTAL/UNSOUND:
//
// We compensate in an unsafe way for the problems in run_server by making sure there are
// P accept loops.  The problem is that all of these are blocking and that is not what we
// want the Cilk workers doing.  MAYBE we would want a cilk worker to try a non-blocking
// accept followed by going to steal.
void run_P(int P, int base, int sockfd) 
// Parallel spawn: Only works for powers of two.
{
    printf("Spawning parallel server accept loops, base %d,  P %d\n", base, P);
    if (P == 1) 
        run_server(base, sockfd);
    else {
        cilk_spawn run_P(P>>1, base,        sockfd);
                   run_P(P>>1, base + P>>1, sockfd);
    }
}

void print_usage(char* name) {
    printf("Usage: %s mode P \n", name);
    printf("   Mode is one of: \n");
    printf("     noop - simply accept socket connections at max rate\n");
    printf("     pong - one immediate response on each connection\n");
    exit(-1);
}


//====================================================================================================

// Alternatively we can have libevent/evhttp handle accepts

// WARNING: This approach will only work if it is ok for the event_dispatch loop to
// MIGRATE THREADS DUE TO WORK STEALING!
// This is one of those places where it would really be nice to have the option of
// child-stealing.
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

//====================================================================================================

int main(int argc, char **argv) 
{
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
    struct evhttp *httpd = NULL;
    global_base = event_init();
    printf(" *** Event system initialized!\n");

    if (global_base == NULL) { printf("INIT FAILED!\n"); return -1; }
    httpd = evhttp_new(global_base);
    if (httpd == NULL) { printf("evhttp_new failed\n"); return -1; }
    if (evhttp_bind_socket(httpd, "127.0.0.1", port) != 0) 
       { printf("evhttp_bind_socket failed!\n"); return -1; }
    evhttp_set_gencb(httpd, process_request, NULL);
    printf(" *** Going into event loop...\n");
    event_base_dispatch(global_base);
    printf(" *** OUT OF EVENT LOOP\n");    

    return 0;
}

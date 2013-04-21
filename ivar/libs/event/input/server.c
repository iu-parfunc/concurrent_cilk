#include "../event_cilk.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cilk/cilk.h>

// Globals

static const int port = 5002;
static const int units = 5;
static const int iters = 10 * 1000 * 1000;
static const int client_limit = 4;

volatile int keep_going = 1;
volatile int total_served = 0;

int do_reads = 0;

const char* response = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n";

// end globals


// non-cilk/ivars specific

void sigchld_handler(int s) {
  while(waitpid(-1, NULL, WNOHANG) > 0);
}

// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa) {
  if (sa->sa_family == AF_INET) 
    return &(((struct sockaddr_in*)sa)->sin_addr);
  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}


// end













int reader(event *e1) {
  uint64_t sum = ((event_data_t *) event_wait(e1->self))->u64;

  int i = e1->ndeps-1;
  for(i; i >= 0; i--)
    sum += ((event_data_t *) e1->deps[i])->u64;
  
  return sum;
}

void writer(event *e, uint64_t *num) {
  event_fire(e->self, (event_data_t *) num);
}

void input_writer(event *e) {
  printf("enter a number: ");
  char buf[256];
  memset(&buf,0,256);
  gets(buf);
  uint64_t *num = (uint64_t *) malloc(sizeof(uint64_t));
  *num = (uint64_t) atoi(buf);
  event_fire(e->self, (event_data_t *) num);
}

int main(void) {

  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;
  event_init();
  event *e1 = event_create();
  event *e2 = event_create();
  event *e3 = event_create();

  event_ctl(e1->self, e2->self, ADD);
  event_ctl(e1->self, e3->self, ADD);

  var = cilk_spawn reader(e1);

  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e2, &v2);
  cilk_spawn input_writer(e3);



  cilk_sync;
  printf("value: = %d\n", var);

  return 0;
}


/*
int main (void){

  int sockfd, new_fd;  // listen on sock_fd, new connection on new_fd
  struct addrinfo hints, *servinfo, *p;
  struct sockaddr_storage their_addr; // connector's address information
  socklen_t sin_size;
  struct sigaction sa;
  int yes=1;
  char s[INET6_ADDRSTRLEN];
  int rv;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE; // use my IP
  if ((rv = getaddrinfo(NULL, PORT, &hints, &servinfo)) != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
    return 1;
  }


  for(p = servinfo; p != NULL; p = p->ai_next) {
    if ((sockfd = socket(p->ai_family, p->ai_socktype,
            p->ai_protocol)) == -1) {
      perror("server: socket");
      continue;
    }
    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes,
          sizeof(int)) == -1) {
      perror("setsockopt");
      exit(1);
    }
    if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
      close(sockfd);
      perror("server: bind");
      continue;
    }
    break;
  }
  if (p == NULL)  {
    fprintf(stderr, "server: failed to bind\n");
    return 2;
  }

  freeaddrinfo(servinfo); // all done with this structure
  if (listen(sockfd, BACKLOG) == -1) {
    perror("listen");
    exit(1);
  }
  sa.sa_handler = sigchld_handler; // reap all dead processes
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART;
  if (sigaction(SIGCHLD, &sa, NULL) == -1) {
    perror("sigaction");
    exit(1);
  }
  printf("server: waiting for connections...\n");
  while(1) {  // main accept() loop
    sin_size = sizeof their_addr;
    new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
    if (new_fd == -1) {
      perror("accept");
      continue;
    }
    inet_ntop(their_addr.ss_family,
        get_in_addr((struct sockaddr *)&their_addr),
        s, sizeof s);
    printf("server: got connection from %s\n", s);
    if (!fork()) { // this is the child process
      close(sockfd); // child doesn't need the listener
      if (send(new_fd, "Hello, world!", 13, 0) == -1)
        perror("send");
      close(new_fd);
      exit(0);
    }
    close(new_fd);  // parent doesn't need this
  }
  return 0;
}






} // close main
*/
/*
int main(void) {

  uint64_t v1 = 39;
  uint64_t v2 = 1;
  uint64_t v3 = 5;
  int var;
  event_init();
  event *e1 = event_create();
  event *e2 = event_create();
  event *e3 = event_create();

  event_ctl(e1->self, e2->self, ADD);
  event_ctl(e1->self, e3->self, ADD);

  var = cilk_spawn reader(e1);

  cilk_spawn writer(e1, &v1);
  cilk_spawn writer(e2, &v2);
  cilk_spawn input_writer(e3);



  cilk_sync;
  printf("value: = %d\n", var);

  return 0;
}
*/

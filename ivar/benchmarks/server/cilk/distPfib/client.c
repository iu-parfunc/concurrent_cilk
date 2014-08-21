#include <sys/types.h>          
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>  
#include <stdio.h>
#include <unistd.h>
#include <cilk/cilk.h>
#include "agreement.h"
#include "common.h"
//#include "itoa.h"
#include "../../../../../common/timer.h"

#define NUM_SERVERS 19

char* nameList[NUM_SERVERS] = {"basalt.cs.indiana.edu", "breccia.cs.indiana.edu", "chert.cs.indiana.edu", //"coal.cs.indiana.edu", 
  "dacite.cs.indiana.edu", "diorite.cs.indiana.edu", "gabbro.cs.indiana.edu", "gneiss.cs.indiana.edu",
  "granite.cs.indiana.edu", "limestone.cs.indiana.edu", "marble.cs.indiana.edu", "onyx.cs.indiana.edu",
  "phyllite.cs.indiana.edu", "quartzite.cs.indiana.edu", "sandstone.cs.indiana.edu", "schist.cs.indiana.edu",
  "shale.cs.indiana.edu", "siltstone.cs.indiana.edu", "slate.cs.indiana.edu", "travertine.cs.indiana.edu"};

void makeConnection(int socketID, char* serverName, in_port_t port){
  struct hostent* ph;
  struct sockaddr_in serverInfo;
  struct sockaddr_in mySockInfo;
  socklen_t sockaddr_inLen;

  memset(&serverInfo, 0, sizeof(struct sockaddr_in));

  if ((ph = gethostbyname(serverName))==NULL){
    fprintf(stderr, "%s\n", hstrerror(h_errno));
    exit(1);
  }

  memcpy((char*)&serverInfo.sin_addr, ph->h_addr_list[0], ph->h_length);

  serverInfo.sin_port = htons(port);
  serverInfo.sin_family = AF_INET;

  if(connect(socketID, (struct sockaddr*)&serverInfo, 
        sizeof(struct sockaddr_in))<0){
    perror("connect");
    exit(1);
  }


  sockaddr_inLen = sizeof(struct sockaddr_in);

  if(getsockname(socketID, (struct sockaddr*)&mySockInfo,
        &sockaddr_inLen)<0){
    perror("getsockname");
    exit(1);
  }

  printf("connected to server through port: %d\n", 
      ntohs(mySockInfo.sin_port));

} 

/* DEPS:
 * Global: nameList, NUM_SERVERS
 */
int* initReduce(){
  // make an array for all of our sockets
  int sockIDs[NUM_SERVERS];
  int i;

  // setup all the sockets we need
  for (i = 0; i < NUM_SERVERS; i++){
    sockIDs[i] = openSocket();
    printf("connecting.... %s\n", nameList[i]);
    makeConnection(sockIDs[i], nameList[i], PORT);
  }
  return sockIDs;
}

void closeReduce(int* sockIDs){
  int i;
  for(i = 0; i < NUM_SERVERS; i++){
    printf("closing %d\n", sockIDs[i]);
    close(sockIDs[i]);
  }
}

int runMap(int socketID, int num){
  frame f;
  my_timer_t t;

  f.data = num;
  printf("calling %d..\n", socketID);
  TIMER_START(t);
  sendPacket(socketID, &f, sizeof(f));

  recvPacket(socketID, &f, sizeof(f));
  TIMER_STOP(t);

  printf("returned: %d\ttime: %f\tsocket: %d\n", f.data, TIMER_EVAL(t), socketID);
  return f.data;
} 

long pfib(int n) {
  if (n<2) return 1;

  long x;
  long y;
  x = cilk_spawn pfib(n-1);
  // We add a spawn here in order to match time_fib in qthreads
  y = cilk_spawn pfib(n-2);

  cilk_sync;
  return x+y;
}

// Use IO lib here in order to make it not lose paralelism on the blocking calls over the network
long distPfib(int n, int* socketID){
  if (n < 2) return 1;
  long x, y;

  // no matter which way we order these lines, we still wind up in the 
  // 12.--- second range
  y = cilk_spawn distPfib(n - 2, socketID);
  x = cilk_spawn runMap(socketID[rand() % NUM_SERVERS], n-1);
  
  return x + y;
}

int main(int argc, char** argv){

  int n;
  if (argc > 1)
    n = atoi(argv[1]);
  else
    n = 40;

  int socketID;
  long j;
  int i;
  int arr[NUM_SERVERS];
  int* sockIDs = initReduce();

  my_timer_t t;
  TIMER_START(t);
  //j = distPfib(n, sockIDs, 0);
  j = distPfib(n, sockIDs);
  TIMER_STOP(t);
  printf("computed distFib of %d\treturned: %lu\ttime: %4f\n", n, j, TIMER_EVAL(t));

  return 0;
}



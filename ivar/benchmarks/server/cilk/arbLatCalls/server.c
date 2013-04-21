#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>
//#include <pthread.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

#include "agreement.h"
#include "common.h"

void bindSocket(int, in_port_t);
void listenOnSocket(int);
int acceptConnection(int);
void* handleClient(void*);

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

int main(){

  int socketID;
  int clientSocketID;
  

  printHostName();

  socketID = openSocket();
  bindSocket(socketID, PORT);
  listenOnSocket(socketID);

  for(;;){

    clientSocketID = acceptConnection(socketID);
   
    cilk_spawn handleClient(&clientSocketID);
  }
  
  close(socketID);
  
  return 0;
}

void* handleClient(void* arg){
  
  frame f;
  long ret;
  char buffer[33];
  int clientSocketID = *((int*)arg);
 
  
  while(recvPacket(clientSocketID, &f, sizeof(f))>0){
      
    printf("client says: %d\n", f.data);// atoi(f.data));
    ret = pfib(f.data);//pfib(atoi(f.data));
    //strcpy(f.data,itoa(ret)); // might need to make these a long eventually
    f.data = (int)ret;
    sendPacket(clientSocketID, &f, sizeof(f));
    
  }

  

  close(clientSocketID);

  return NULL;

} 

void bindSocket(int socketID, in_port_t port){

   struct sockaddr_in sa;
  
   memset(&sa,0,sizeof(struct sockaddr_in));
  
   sa.sin_family = AF_INET;
   sa.sin_port = htons(port);
   sa.sin_addr.s_addr = htonl(INADDR_ANY);
  
   if(bind(socketID, (struct sockaddr*)&sa, sizeof(struct sockaddr_in))<0){
      perror("bind");
      exit(1);
   }
  
}



void listenOnSocket(int socketID){
  if(listen(socketID, 5)<0){
    perror("listen");
    exit(1);
  }
} 

int acceptConnection(int listeningSocket){

  struct hostent *he;

  int clientSocketID;
  struct sockaddr_in clientInfo;
      
  unsigned int clientInfoSize = sizeof(struct sockaddr_in);
      
  printf("waiting to be contacted for transferring files...\n");
      
  if((clientSocketID=accept(listeningSocket, (struct sockaddr*)&clientInfo,
          &clientInfoSize))<0){
    perror("accept");
    exit(1);
  }
      
  if((
      he=gethostbyaddr(&clientInfo.sin_addr,
           sizeof(clientInfo.sin_addr.s_addr),
           AF_INET))==NULL){
      
     
    fprintf(stderr,"%s\n", hstrerror(h_errno));
      
      
  }
      
  printf("got connection from: %s:%d\n", he->h_name,
   ntohs(clientInfo.sin_port));
      
      
  return clientSocketID;
      
}

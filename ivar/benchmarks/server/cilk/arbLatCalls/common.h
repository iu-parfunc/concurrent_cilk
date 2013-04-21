/*
#ifndef COMMON_H
#define COMMON_H

#define HOST_NAME_MAX 256

void printHostName(void);
int openSocket(void);
size_t recvPacket(int, const void *, size_t);
size_t sendPacket(int, const void *, size_t);

#endif
*/
#ifndef COMMON_H
#define COMMON_H

#define HOST_NAME_MAX 256

#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>


void printHostName(void){
      
  char name[256];
      
  if(gethostname(name, 256)<0){
    perror("hostname");
    exit(1);
  }
      
  printf("starting at host [%s]\n", name);
      
}
    

int openSocket(){

  int socketID;

  socketID = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if(socketID<0){
    perror("socket");
    exit(1);
  }

  return socketID;
    
}      
      
    
      
size_t recvPacket(int socketID, const void *p, size_t size){

  int nread;
  char* ptr;
  int nleft;


  ptr = (char*)p;

  nleft = size;
  
  do{
    nread = recv(socketID, ptr, nleft, 0);
    
    nleft -= nread;
    ptr+=nread;
    
  }while(nleft>0 && nread!=0);

  return (size-nleft);

}

size_t sendPacket(int socketID, const void *p, size_t size){

  int nleft;
  int nwrite;
  char* ptr;

  ptr = (char*)p;

  nleft = size;

  do{
    nwrite = send(socketID, ptr, nleft, 0);

    nleft -= nwrite;
    ptr+=nwrite;

  }while(nleft>0 && nwrite!=0);

  return (size-nwrite);
}


#endif


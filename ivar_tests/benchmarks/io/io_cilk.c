#include "io_cilk.h"
#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>


int cilk_read(int fd, char *buf, int nbytes, __cilkrts_ivar iv) {
  int bytes = nbytes, tmp = 0;
  
  //figure that we are going to block,
  //let the continuation get stolen
  printf("in read\n");

  //read in serial until we get nbytes
  while(bytes > 0) {
//    printf("in read...%d\n", bytes);
    tmp = read(fd, buf, bytes);

    //return any errors/EOF
    if(tmp < 0){
      perror("ERROR IN READ:");
      __cilkrts_ivar_write(&iv, (ivar_payload_t) -1);
      return tmp;
    }

    buf   += tmp; //increment pointer to buffer
    bytes -= tmp; //subtract the bytes read
  }

    printf("read done...%d\n", bytes);
  //wake the stack, read finished
  __cilkrts_ivar_write(&iv, (ivar_payload_t) 1);

  return nbytes;
}

int cilk_write(int fd, char *buf, int nbytes, __cilkrts_ivar iv) {
  int bytes = nbytes, tmp = 0;
  
  printf("in write\n");
  //figure that we are going to block,
  //let the continuation get stolen

  //write in serial until we get nbytes
  while(bytes > 0) {
 // printf("in write...%d\n", bytes);
    tmp = write(fd, buf, bytes);

    printf("wrote...%d\n", tmp);
    //return any errors
    if(tmp < 0){
      perror("ERROR IN WRITE:");
      __cilkrts_ivar_write(&iv, (ivar_payload_t) -1);
      return tmp;
    }

    buf   += tmp; //increment pointer to buffer
    bytes -= tmp; //subtract the bytes written
  }

  printf("after write...waking stack\n");
  //wake the stack, write finished
  __cilkrts_ivar_write(&iv, (ivar_payload_t) 1);

  return nbytes;
}

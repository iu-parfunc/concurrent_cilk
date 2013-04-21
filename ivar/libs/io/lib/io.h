#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
/*
A simple IO library using Ivars. Some of these might be able to be sped up by using a sys_call queue
*/


// *iv is already cleared
//int cilk_accept(int socket_descriptor, struct sockaddr *address, int *address_length, Ivar *iv)
#define cilk_accept(socket_descriptor, address, address_length, iv) {\
  cilk_spawn __cilkrts_ivar_read(iv);  \
  int tmp = accept(socket_descriptor, address, address_length);\
  __cilkrts_ivar_write(iv, tmp);\
}

// *iv is already cleared  
// int cilk_connect (int socket,  struct sockaddr *restrict address,  socklen_t *restrict address_len, Ivar *iv);
#define cilk_connect(socket, address, address_len, iv) {\
  cilk_spawn __cilkrts_ivar_read(iv);\
  int tmp = connect(socket, address, address_len);\
  __cilkrts_ivar_write(iv, tmp);\
}

//poll is not included. (IMHO it doesnt make sense uless we decide to add in a sys-call queue)

// *iv is already cleared  
//ssize_t cilk_pread(int fildes, void *buf, size_t nbyte, off_t offset, Ivar *iv); 
#define cilk_pread(fildes, buf, nbyte, offset, iv) {\
  cilk_spawn __cilkrts_ivar_read(iv);\
  ssize_t tmp = pread(fildes, buf, nbyte, offset);\
  __cilkrts_ivar_write(iv, tmp);\
  
}

// *iv is already cleared  
//ssize_t read(int fildes, void *buf, size_t nbyte);
#define cilk_pread(fildes, buf, nbyte, iv) {\
  cilk_spawn __cilkrts_ivar_read(iv);\
  ssize_t tmp = pread(fildes, buf, nbyte);\
  __cilkrts_ivar_write(iv, tmp);\
}

// not doing pwrite and write. 

/* TODO:
 * select
 * system 
 * wait4
 * add in sys_queue
 */


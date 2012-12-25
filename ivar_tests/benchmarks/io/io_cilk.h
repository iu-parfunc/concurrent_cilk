#ifndef _io_cilk_h_
#define _io_cilk_h_

#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

//__cilkrts_ivar payload is just a machine word
typedef void* ivar_payload_t;

int cilk_read(int fd, char *buf, int nbytes);
int cilk_write(int fd, char *buf, int nbytes);

#endif

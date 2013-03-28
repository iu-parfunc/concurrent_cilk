#include <cilk/common.h>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <cilk/concurrent_cilk.h>
#include <libevent.h>
#include <event.h>

struct event_base *base = event_base_new();


int cilk_read(int fd, char* buf, int nbytes){

}





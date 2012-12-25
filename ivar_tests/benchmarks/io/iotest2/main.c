#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "../io_cilk.h"
#include "../../event_cilk.h" // might be in a different place...


/*
void input_writer(event *e1, event *e) {
  printf("enter a number: ");
  char buf[256];
  memset(&buf,0,256);
  if(gets(buf) !=0){
 
    int len = strlen(buf);
    char *rbuf = (char *)calloc(len+1, sizeof(char));
    int fd = open("tmp_text.txt", O_CREAT | O_RDWR | O_TRUNC, 5555);

    event *e2 = event_create();
    event_ctl(e1->self, e->self, ADD);
    //  event_ctl(e1->self, e2->self, ADD);
    event_fire(e->self, (event_data_t *) num);
    printf("written\n");
    input_writer(e1, e2);
  }
}
*/


void input_writer(event *e) {
  char buf[256];
  memset(&buf,0,256);
  gets(buf);
  
  lseek(fd, 0, SEEK_SET);

  cilk_write(fd,buf,len);
  
  event_fire(e->self, (event_data_t *) num);

}






int main(void) {

  int fd = open("tmp_text.txt", O_CREAT | O_RDWR | O_TRUNC, 5555);
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
  cilk_spawn input_writer(e1,e3);

  cilk_sync;
  printf("value: = %d\n", var);

  close(fd);
  return 0;
}





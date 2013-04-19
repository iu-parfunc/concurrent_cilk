// TODO: write now this is writing garbage... FIX
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "../io_cilk.h"

#define NUM 10000

// fix this!
char* read_from_file(char *filename, size_t size, int len, __cilkrts_ivar iv) {
  int fd;
  char *buffer=(char*) calloc(size, sizeof(char));
  fd = open(filename, O_RDONLY);
  if (fd == -1) {
    printf("\nFile Open Unsuccessful\n");
    exit (0);;
  }
  lseek(fd,0,SEEK_SET);
    size_t readnow;
    readnow = cilk_spawn cilk_read(fd, buffer, len, iv);
    if (readnow < 0 ) {
      printf("\nRead Unsuccessful\n");
      close (fd);
      exit(0);
    }

  close(fd);
  return buffer;
}

int write_to_file(char *filename, size_t size, char* b,  __cilkrts_ivar iv){
  int fd;
  fd = open(filename, O_WRONLY | O_APPEND);
  int len = strlen(b);
//  printf("\t\t writing %s\n", b);
  if (fd == -1)
  {
    printf("\nFile write Unsuccessful\n");
    exit (0);;
  }
  lseek(fd,0,SEEK_SET);

    size_t written;
    written = cilk_write(fd,b, len, iv);
    if (written < 0 ) {
      printf("\nwrite Unsuccessful\n");
      close (fd);
      return 1;
    }

  close(fd);
  return 1;
}



int main (){

  int i;
  // already initialized to NULL.. so they are already cleared
  __cilkrts_ivar *ivar_array = (__cilkrts_ivar *) calloc(2*NUM+1, sizeof(__cilkrts_ivar));
  // make an array of all the files we want to read from
  char** read_filebuf = (char **) calloc(NUM+1, sizeof(char*));
  // Were condensing into one file. So yea.. (might want array of files here??

  char* final_file = "foo.out";
  for (i = 0; i <= NUM; i++)
    read_filebuf[i] ="foo1.in"; 
  //read_filebuf[0] = "foo1.in";
  //read_filebuf[1] = "foo2.in";
  
  //cilk_spawn __cilkrts_ivar_read(&ivar_array[NUM+1]); // to tell us when we are done

  // TODO: need to write way to add the files to read_filebuf and final_file

  char* tmp = NULL;
  for(i = 0; i <= 2*NUM; i += 2){
    tmp = cilk_spawn read_from_file(read_filebuf[i/2], sizeof(char*),7, ivar_array[i/2]);
    cilk_spawn write_to_file(final_file, sizeof(char*), tmp, ivar_array[i/2+1]);
    // this will write ivar_array[NUM+1] at the end and signify that were done
  }

  /* With the above were making this diagram with the Ivar dependencies:
   *
   *    wf2  wf3       wf(n+1)       ==> which then says were done
   *   /  \ /   ....  /
   * rf1  rf2       rfn
   *
   */
  cilk_sync;
  return 1;
}



     




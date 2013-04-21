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
char* read_from_file(char *filename, size_t size, int len) {
  int fd;
  char *buffer=(char*) calloc(size, sizeof(char));
  fd = open(filename, O_RDONLY);
  if (fd == -1) {
    printf("\nFile Open Unsuccessful\n");
    exit (0);;
  }
  lseek(fd,0,SEEK_SET);
    size_t readnow;
    readnow = cilk_spawn read(fd, buffer, len);
    if (readnow < 0 ) {
      printf("\nRead Unsuccessful\n");
      close (fd);
      exit(0);
    }

  close(fd);
  return buffer;
}

int write_to_file(char *filename, size_t size, char* b){
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
    written = write(fd,b, len);
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
  // make an array of all the files we want to read from
  char** read_filebuf = (char **) calloc(NUM+1, sizeof(char*));
  // Were condensing into one file. So yea.. (might want array of files here??

  char* final_file = "foo.out";
  for (i = 0; i <= NUM; i++)
    read_filebuf[i] ="foo1.in"; 
  //read_filebuf[0] = "foo1.in";
  //read_filebuf[1] = "foo2.in";
  

  // TODO: need to write way to add the files to read_filebuf and final_file

  char* tmp = NULL;
  for(i = 0; i <= 2*NUM; i += 2){
    tmp = cilk_spawn read_from_file(read_filebuf[i/2], sizeof(char*),7);
    cilk_spawn write_to_file(final_file, sizeof(char*), tmp);
    // this will write ivar_array[NUM+1] at the end and signify that were done
  }

  cilk_sync;
  return 1;
}



     




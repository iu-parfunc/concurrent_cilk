#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "../io_cilk.h"

int main(void) {
  char *buf = "Sing, O goddess, the anger of Achilles son of Peleus, that brought countless ills upon the Achaeans. Many a brave soul did it send hurrying down to Hades, and many a hero did it yield a prey to dogs and vultures, for so were the counsels of Jove fulfilled from the day on which the son of Atreus, king of men, and great Achilles, first fell out with one another. And which of the gods was it that set them on to quarrel? It was the son of Jove and Leto; for he was angry with the king and sent a pestilence upon the host to plague the people, because the son of Atreus had dishonoured Chryses his priest. Now Chryses had come to the ships of the Achaeans to free his daughter, and had brought with him a great ransom: moreover he bore in his hand the sceptre of Apollo wreathed with a suppliant's wreath and he besought the Achaeans, but most of all the two sons of Atreus, who were their chiefs. Sons of Atreus, he cried, and all other Achaeans, may the gods who dwell in Olympus grant you to sack the city of Priam, and to reach your homes in safety; but free my daughter, and accept a ransom for her, in reverence to Apollo, son of Jove." ;

  int len = strlen(buf);
 // char *rbuf = (char *) calloc(len+1, sizeof(char));
  int fd  = open("tmp_test_cilk_io.txt", O_CREAT | O_RDWR | O_TRUNC, 5555);

//   cilk_spawn cilk_read(fd, rbuf, len);
   sleep(1);
   lseek(fd, 0, SEEK_SET);
   __cilkrts_ivar iv;
   __cilkrts_ivar_clear(&iv);
   cilk_write(fd, buf, len, iv);
   close(fd);
  // printf("result of reading file: %s \n",rbuf);
  // free(rbuf);

  return 0;
}

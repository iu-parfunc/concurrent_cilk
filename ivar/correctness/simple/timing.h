#include <time.h>

void __cilkrts_msleep(unsigned long millis)
{
  struct timespec time;
  time.tv_sec = millis / 1000;
  time.tv_nsec = (millis % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

void __cilkrts_usleep(unsigned long micros)
{
  struct timespec time;
  time.tv_sec = micros/1000000ul;
  time.tv_nsec = (micros % 1000) * 1000000ul;
  nanosleep(&time,NULL);
}

#ifndef TIMER_H_
#define TIMER_H_

#include <sys/time.h>
typedef struct {
  double elapsed_time;
  double last_start_time;
} my_timer_t;
//typedef struct my_timer_t my_timer_t;

#define TIMER_RESET(t) { t.elapsed_time = 0.0; }

#define TIMER_START(t)                 \
{                    \
  struct timeval tod;                \
  gettimeofday(&tod, NULL);              \
  t.last_start_time = (double)tod.tv_sec + ((double)tod.tv_usec * 1.0e-6); \
}
#define TIMER_STOP(t)                 \
{                     \
  struct timeval tod;                 \
  gettimeofday(&tod, NULL);               \
  t.elapsed_time += ((double)tod.tv_sec + ((double)tod.tv_usec * 1.0e-6)) - \
                      t.last_start_time;            \
}
#define TIMER_EVAL(t) (t.elapsed_time)

#endif

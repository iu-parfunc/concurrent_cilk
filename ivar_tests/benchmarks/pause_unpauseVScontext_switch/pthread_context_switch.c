#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/syscall.h>
#include <sys/wait.h>
//#include <time.h>
#include <unistd.h>
#include "../../common/cycle.h"
#include "../../../timer.h"
// linux dependent
#include <linux/futex.h>

static const int iterations = 250;

static void* thread(void* restrict ftx) {
  int* futex = (int*) ftx;
  for (int i = 0; i < iterations; i++) {
    sched_yield();
    while (syscall(SYS_futex, futex, FUTEX_WAIT, 0xA, NULL, NULL, 42)) {
      // retry
      sched_yield();
    }
    *futex = 0xB;
    while (!syscall(SYS_futex, futex, FUTEX_WAKE, 1, NULL, NULL, 42)) {
      // retry
      sched_yield();
    }
  }
  return NULL;
}

int main() {
  ticks start, end;
  my_timer_t t;
  const int shm_id = shmget(IPC_PRIVATE, sizeof (int), IPC_CREAT | 0666);
  int* futex = shmat(shm_id, NULL, 0);
  pthread_t thd;
  if (pthread_create(&thd, NULL, thread, futex)) {
    return 1;
  }
  *futex = 0xA;

  TIMER_START(t);
  start = getticks();
  for (int i = 0; i < iterations; i++) {
    *futex = 0xA;
    while (!syscall(SYS_futex, futex, FUTEX_WAKE, 1, NULL, NULL, 42)) {
      // retry
      sched_yield();
    }
    sched_yield();
    while (syscall(SYS_futex, futex, FUTEX_WAIT, 0xB, NULL, NULL, 42)) {
      // retry
      sched_yield();
    }
  }
  end = getticks();
  TIMER_STOP(t);

  const int nswitches = iterations << 2;
  //printf("%i  thread context switches in %lluns (%.1fns/ctxsw)\tticks per ctxsw: %lf\n",
    //     nswitches, delta, (delta / (float) nswitches), elapsed(end,start)/ (float) nswitches);
  printf("%lf\n", elapsed(end,start)/ (float) nswitches);
  //printf("switches: %d\n", nswitches);
  wait(futex);
  return 0;
}

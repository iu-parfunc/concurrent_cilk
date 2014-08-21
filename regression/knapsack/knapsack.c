/*
 * Cilk program to solve the 0-1 knapsack problem using a branch-and-bound
 * technique. 
 *
 * Author: Matteo Frigo
 */
//static const char *ident __attribute__((__unused__))
//  = "$HeadURL: https://bradley.csail.mit.edu/svn/repos/cilk/5.4.3/examples/knapsack.cilk $ $LastChangedBy: sukhaj $ $Rev: 517 $ $Date: 2003-10-27 10:05:37 -0500 (Mon, 27 Oct 2003) $";
/*
 * Copyright (c) 2000 Massachusetts Institute of Technology
 * Copyright (c) 2000 Matteo Frigo
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>
#include "../utils/timer.h"
#include "../utils/getoptions.h"
/* every item in the knapsack has a weight and a value */
#define MAX_ITEMS 256

struct item {
  int value;
  int weight;
};

int best_so_far = INT_MIN;

int compare(struct item *a, struct item *b)
{
  double c = ((double) a->value / a->weight) -
    ((double) b->value / b->weight);

  if (c > 0)
    return -1;
  if (c < 0)
    return 1;
  return 0;
}

int read_input(const char *filename, struct item *items, int *capacity, int *n)
{
  int i;
  FILE *f;

  if (filename == NULL)
    filename = "\0";
  f = fopen(filename, "r");
  if (f == NULL) {
    fprintf(stderr, "open_input(\"%s\") failed\n", filename);
    return -1;
  }
  /* format of the input: #items capacity value1 weight1 ... */
  fscanf(f, "%d", n);
  fscanf(f, "%d", capacity);

  for (i = 0; i < *n; ++i)
    fscanf(f, "%d %d", &items[i].value, &items[i].weight);

  fclose(f);

  /* sort the items on decreasing order of value/weight */
  /* cilk2c is fascist in dealing with pointers, whence the ugly cast */
  qsort(items, *n, sizeof(struct item),
      (int (*)(const void *, const void *)) compare);

  return 0;
}

/* 
 * return the optimal solution for n items (first is e) and
 * capacity c. Value so far is v.
 */
int knapsack(struct item *e, int c, int n, int v)
{
  int with, without, best;
  double ub;

  /* base case: full knapsack or no items */
  if (c < 0)
    return INT_MIN;

  if (n == 0 || c == 0)
    return v;		/* feasible solution, with value v */

  ub = (double) v + c * e->value / e->weight;

  if (ub < best_so_far) {
    /* prune ! */
    return INT_MIN;
  }
  /* 
   * compute the best solution without the current item in the knapsack 
   */
  without = cilk_spawn knapsack(e + 1, c, n - 1, v);

  /* compute the best solution with the current item in the knapsack */
  with = cilk_spawn knapsack(e + 1, c - e->weight, n - 1, v + e->value);

  cilk_sync;

  best = with > without ? with : without;

  /* 
   * notice the race condition here. The program is still
   * correct, in the sense that the best solution so far
   * is at least best_so_far. Moreover best_so_far gets updated
   * when returning, so eventually it should get the right
   * value. The program is highly non-deterministic.
   */
  if (best > best_so_far)
    best_so_far = best;

  return best;
}

int usage(void)
{
  fprintf(stderr, "\nUsage: knapsack [<cilk-options>] [-f filename] [-benchmark] [-h]\n\n");
  fprintf(stderr, "The 0-1-Knapsack is a standard combinatorial optimization problem: ``A\n");
  fprintf(stderr, "thief robbing a store finds n items; the ith item is worth v_i dollars\n");
  fprintf(stderr, "and weighs w_i pounds, where v_i and w_i are integers. He wants to take\n");
  fprintf(stderr, "as valuable a load as possible, but he can carry at most W pounds in\n");
  fprintf(stderr, "his knapsack for some integer W. What items should he take?''\n\n");
  return -1;
}

char *specifiers[] =
{"-f", "-benchmark", "-h", 0};
int opt_types[] =
{STRINGARG, BENCHMARK, BOOLARG, 0};

int main(int argc, char *argv[])
{
  struct item items[MAX_ITEMS];	/* array of items */
  int n, capacity, sol, benchmark, help;
  char filename[100];

  timer_t t;
  /* standard benchmark options */
  strcpy(filename, "knapsack-example3.input");

  get_options(argc, argv, specifiers, opt_types, filename, &benchmark, &help);

  if (help)
    return usage();

  if (benchmark) {
    switch (benchmark) {
      case 1:		/* short benchmark options -- a little work */
        strcpy(filename, "knapsack-example1.input");
        break;
      case 2:		/* standard benchmark options */
        strcpy(filename, "knapsack-example2.input");
        break;
      case 3:		/* long benchmark options -- a lot of work */
        strcpy(filename, "knapsack-example3.input");
        break;
    }
  }
  if (read_input(filename, items, &capacity, &n))
    return 1;

  /* Timing. "Start" timers */
  cilk_sync;

  TIMER_START(t);
  sol = cilk_spawn knapsack(items, capacity, n, 0);
  cilk_sync;
  TIMER_STOP(t);


  printf("\nCilk Example: knapsack\n");
  printf("options: problem-file = %s\n\n", filename);
  printf("Best value is %d\n\n", sol);
  printf("Running time  = %4f s\n", TIMER_EVAL(t));

  return 0;
}

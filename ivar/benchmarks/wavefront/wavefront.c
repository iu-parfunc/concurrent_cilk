#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include <timer.h>
#include <cycle.h>
#include <string.h>

#define TILE_DIM 2
#define MATRIX_DIM 2

typedef ivar_payload_t tile[TILE_DIM*TILE_DIM];
typedef ivar_t *matrix[MATRIX_DIM*MATRIX_DIM];


void sum_tile(tile *src, tile *dst) 
{
  int i;
  for(i=0; i < TILE_DIM * TILE_DIM; i++) { dst[i] += src[i]; } 
}

void init_tile(tile *t)       { memset(t, 1, TILE_DIM*TILE_DIM); }
void init_matrix(matrix *mat) { memset(mat, 0, MATRIX_DIM*MATRIX_DIM); } 

void do_tile(matrix *mat, int i, int j) 
{
  ivar_t *cur_tile = mat[i * j];
  tile *src; 
  tile *dst = (tile *) malloc(sizeof(tile));
  memset(dst, 1, sizeof(tile));

  //add the tile diagonally above to the current tile.
  if(i>0 && j>0) {
     src = ivar_read(mat[(i-1) * (j-1)]);
     sum_tile(src, dst);
  }

  //if possible add the tile above the current tile.
  if (i>0) {
    src = ivar_read(mat[(i-1)* j]);
    sum_tile(src, dst);
  } 

  //if possible add the tile to the left of the current tile
  if (j>0) {
    src = ivar_read(mat[i * (j-1)]);
    sum_tile(src, dst);
  }

  ivar_put(cur_tile, dst);
}

//rows and column elements are computed in parallel.
void traverse_by_par_row_and_column(matrix *mat) {
  int i,j;
  for (i=0; i < MATRIX_DIM; i++)  {
    for (j=0; j < MATRIX_DIM; j++)  {
      cilk_spawn do_tile(mat,i,j);
    }
  }
}

int main(int argc, char **argv) {
  matrix mat;
  my_timer_t t;
  ticks start, end;
  unsigned long long sum;

  init_matrix(&mat);

  TIMER_START(t);
  start = getticks(); 
  traverse_by_par_row_and_column(&mat);
  end = getticks();
  TIMER_STOP(t);

  cilk_sync;
  printf("%i\t%f\t%lf\n", MATRIX_DIM, TIMER_EVAL(t), elapsed(end,start));
  return 0;
}








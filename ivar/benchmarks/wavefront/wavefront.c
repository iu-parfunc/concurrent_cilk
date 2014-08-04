#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>
#include <cilk/concurrent_cilk.h>
#include <timer.h>
#include <cycle.h>
#include <string.h>

#define TILE_DIM 16
#define MATRIX_DIM 64

typedef ivar_payload_t tile;
typedef ivar_t matrix;


void sum_tile(tile *src, tile *dst) 
{
  int i;
  for(i=0; i < TILE_DIM * TILE_DIM; i++) {
    dst[i] += src[i];
  } 
}

void init_tile(tile *t)       { memset(t, 1, TILE_DIM*TILE_DIM); }
void init_matrix(matrix *mat) { 
  int i;
  for(i=0; i < MATRIX_DIM*MATRIX_DIM; i++) {
    clear_iv(&(mat[i]));
  };
} 

void do_tile(matrix *mat, int i, int j) 
{
  ivar_t *cur_tile = &(mat[i * MATRIX_DIM + j]);
  tile *src; 
  tile *dst = (tile *) malloc(sizeof(tile) * TILE_DIM * TILE_DIM);
  memset(dst, 1, sizeof(tile) * TILE_DIM * TILE_DIM);

  //add the tile diagonally above to the current tile.
  if(i>0 && j>0) {
     src = (tile *) read_iv(&(mat[(i-1) * MATRIX_DIM + (j-1)]));
     sum_tile(src, dst);
  }

  //if possible add the tile above the current tile.
  if (i>0) {
    src = (tile *) read_iv(&(mat[(i-1)* MATRIX_DIM + j]));
    sum_tile(src, dst);
  } 

  //if possible add the tile to the left of the current tile
  if (j>0) {
    src = (tile *) read_iv(&(mat[i * MATRIX_DIM + (j-1)]));
    sum_tile(src, dst);
  }

  printf("cur tile %p val %ld idx %i\n", cur_tile, *cur_tile, i*MATRIX_DIM + j);
  write_iv(cur_tile, (ivar_payload_t) dst);
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
  matrix mat[MATRIX_DIM*MATRIX_DIM];
  my_timer_t t;
  ticks start, end;
  unsigned long long sum;

  init_matrix((matrix *) &mat);
  printf("mat %p\n", &mat);

  TIMER_START(t);
  start = getticks(); 
  traverse_by_par_row_and_column((matrix *) &mat);
  end = getticks();
  TIMER_STOP(t);

  cilk_sync;
  printf("%i\t%f\t%lf\n", MATRIX_DIM, TIMER_EVAL(t), elapsed(end,start));
  return 0;
}








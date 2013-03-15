// A mockup of a dynamic-programming style computation.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#include "../../../timer.h"
#include "../../common/cycle.h"
using namespace std;

// Globals:
int dim = 2;
int tiledim = 2;
int reps = 1; // An intensifying coefficient.
char* version = "parfor2";

static const int dbg = 0;

//----------------------------------------

class tile {
public: 
    tile() : vec(tiledim*tiledim)
    {
        for(int i=0; i<tiledim*tiledim; i++)
          vec[i] = 1;
    }

    vector<int> vec;
};


template <typename T>
class myvar{

  public:
    void put(const T& data){
      val=data;
    }

    const T& get(){
      return val; 
    }

  private:
    T val;
};

//----------------------------------------

// Globals:
vector< myvar <tile*> >* matrix;

//----------------------------------------

tile* matrix_get(int i, int j) {
    if(dbg) printf("  matrix get: %d %d\n", i,j);
    return (*matrix)[i * dim + j].get();
}

void matrix_put(int i, int j, tile* val) {
    if(dbg) printf("  matrix put: %d %d\n", i,j);
    return (*matrix)[i * dim + j].put(val);
}

void sum_tile(tile* dest, tile* src) {
    for(int r=0; r<reps; r++) // An intensifying coefficient.
    for(int i=0; i< tiledim * tiledim; i++) {
        dest->vec[i] += src->vec[i];
    }
}

// Serially sum up everything in the matrix:
unsigned long long sum_matrix() 
{
    unsigned long long sum = 0;
    for(int i=0; i< dim*dim; i++) {
        tile* t = (*matrix)[i].get();
        for(int j=0; j< tiledim * tiledim; j++) 
           sum += t->vec[j];
    }
    return sum;
}

// Get the dependencies and do the tile computation.
void do_tile(int i, int j) 
{
    if(dbg) printf("  do tile %d %d\n", i, j);
    tile* t = new tile();
    if(i>0 && j>0) {
        tile* dep1 = matrix_get(i-1,j-1);
        sum_tile(t, dep1);
    }
    if (i>0) {
        tile* dep2 = matrix_get(i-1,j);
        sum_tile(t, dep2);
    } 
    if (j>0) {
        tile* dep3 = matrix_get(i,j-1);
        sum_tile(t, dep3);
    }

    matrix_put(i,j, t);
}

// Version 1:
// Walk over the matrix with naive traversal, spawning all jobs:
void traverse1() 
{
    if(dbg) printf(" *** Begin parallel traversal to fill matrix: cilk_for | cilk_for | spawn\n");
    cilk_for (int i=0; i < dim; i++) 
    cilk_for (int j=0; j < dim; j++) 
       cilk_spawn do_tile(i,j);
    if(dbg) printf("Done with parallel traversal...\n");
}

// Version 1B: Less spawning.
void do_row(int i) {
  for (int j=0; j < dim; j++) 
     do_tile(i,j);
}
void traverse2() 
{
    if(dbg) printf(" *** Begin parallel traversal to fill matrix: for/spawn | for | \n");
    for (int i=0; i < dim; i++) 
      // In this version the theif will steal the next row:
      cilk_spawn do_row(i);
    if(dbg) printf("Done with parallel traversal...\n");
}

// for error checking in divide and conquer
int isPowerOfTwo (unsigned int x)
{
  return ((x != 0) && !(x & (x - 1)));
}

// Version 2:
// In this version we use a divide and conquer that sacrifices parallelism, but no thread
// should EVER block on an ivar.
void divide_and_conquer(int i, int j, int w, int h) 
{
// TODO -- INSERT ERROR MESSAGE IF NOT POWER OF TWO!!!  use popcount if you want ;-).
  if(dbg) printf(" *** Divide and conquer: pos %d,%d dims %dx%d\n", i,j,w,h);
    // ASSUME POWER OF TWO!
    if (w == 1 && h == 1) 
    {
        do_tile(i,j);
    }
    else if (w == 1 || h == 1) 
    {
        if(dbg) fprintf(stderr, "ERROR INVARIANT BROKEN!\n");
        abort();
    } 
    else
    {
        int mid_row = i + w/2;
        int mid_col = j + h/2;
                   divide_and_conquer(i      ,j,       w/2,h/2); // nw 
 // Try with and without a sync here:
        cilk_sync; 
        cilk_spawn divide_and_conquer(mid_row,j,       w/2,h/2); // ne
                   divide_and_conquer(i      ,mid_col, w/2,h/2); // sw
        cilk_sync; 
                   divide_and_conquer(mid_row,mid_col, w/2,h/2); // se 
    }
}

// Version 3: optimistically attempt a nice data partitioning.
// 
// Do cones out from the origin.
// Synchronization will happen at the edges.
void lowertri(int,int, int,int, int,int);
void uppertri(int,int, int,int, int,int);

void spawn_cones() 
{  
    // First a simple two-way split:
    // Orient: Here we use X/Y pixel style indexing:
    
    // WARNING: This traversal breaks the SERIAL PUTS-BEFORE-GETS FIRST rule:
    // It needs either "Full IVars" or an explicit "unordered spawn" type of construct:
    cilk_spawn 
    uppertri(0,0,  dim-1,0,    dim-1,dim-1);
    lowertri(0,1,  dim-2,dim-1,    0,dim-1);
}

/*
    Rasterization of a triangle using scanlines:
      A = y0 - y1
      B = x1 - x0
      C = x0 y1 − x1 y0
      Alternatively:
        C = - (A(x0 +x1)+B(y0 +y1)) / 2

      E(x,y) = Ax + By + C
      If E(x,y) is positive the point is to one side of the line. 
  */

// TODO: Rasterize a triangle using rows/cols:
// Three points are given clockwise from the upper left point.  INCLUSIVE.
void lowertri(int x0, int y0, int x1, int y1, int x2, int y2) 
{
    if(dbg) printf("Rasterizing lower triangle: %d,%d  %d,%d  %d,%d\n", x0,y0,  x1,y1, x2,y2);
    // For loop over the vertical dimension:
    for(int y = y0; y <= y2; y++) 
    {
        // For each scanline, compute the start and end:
        // Start interpolates from x0 to x1:
        int startx = ((x2-x0) * (y-y0)) / (y2-y0);
        int endx   = ((x1-x0) * (y-y0)) / (y2-y0);
        if(dbg) printf("  Drawing horizontal line %d from %d to %d\n", y, startx, endx);
        for(int x=startx; x<=endx; x++) {
            //            do_tile(x,y);
        }
    }
}

void uppertri(int x0, int y0, int x1, int y1, int x2, int y2) 
{
    if(dbg) printf("Rasterizing upper triangle: %d,%d  %d,%d  %d,%d\n", x0,y0,  x1,y1, x2,y2);
    // For loop over the horizontal dimension:
    for(int x = x0; x <= x2; x++)
    {
        // For each scanline, compute the start and end:
        // Start interpolates from x0 to x1:
        int starty = ((y1-y0) * (x-x0)) / (x2-y0);
        int endy   = ((y2-y0) * (x-x0)) / (x2-x0);
        if(dbg) printf("  Drawing vertical line %d from %d to %d\n", x, starty, endy);
        for(int y=starty; y<=endy; y++) {
            //            do_tile(x,y);
        }
    }
}

// ================================================================================
// <UNFINISHED> This doesn't decompose porperly yet.  The helpers need to change the upper
// left start coordinates of the subtriangles.

// Version 3B: Decompose into more than two pieces.
void upper_helper(int decomp, int x0, int y0, int x1, int y1, int x2, int y2) {
    if(dbg) printf("Upper helper: decomp %d   %d,%d  %d,%d  %d,%d\n", decomp, x0, y0, x1, y1, x2, y2);
    if (decomp == 1 || y2-y1 == 1 )
        uppertri(x0, y0, x1, y1, x2, y2);
    else {
        int mid = (y2-y1) / 2;
        // cilk_spawn 
        upper_helper(decomp/2, x0,y0,  x1,y1,     x2,mid);
        upper_helper(decomp/2, x0,y0,  x1,mid+1,  x2,y2);
    }
}

void lower_helper(int decomp, int x0, int y0, int x1, int y1, int x2, int y2) {
    if(dbg) printf("Lower helper: decomp %d   %d,%d  %d,%d  %d,%d\n", decomp, x0, y0, x1, y1, x2, y2);
    if (decomp == 1 || x1-x2 == 1 )
        lowertri(x0, y0, x1, y1, x2, y2);
    else {
        int mid = (x1-x2) / 2;
        // cilk_spawn 
        lower_helper(decomp/2, x0,y0,  mid,y1,  x2,y2);
        lower_helper(decomp/2, x0,y0,  x1,y1,  mid+1,y2);
    }
}

void spawn_cones2()
{
    // TODO set decomposition to K * the # of processors.
    int decomp = 4;
    cilk_spawn upper_helper(decomp/2, 0,0,  dim-1,0,    dim-1,dim-1);
               lower_helper(decomp/2, 0,1,  dim-2,dim-1,    0,dim-1);
}
// </UNFINISHED>
// ================================================================================



// TODO: Rasterize a triangle using diagonals:


int main(int argc, char** argv) 
{
  my_timer_t t;
  ticks start, end;
    if (argc > 5)
    {
        printf("Usage: %s mode matrixDimension tileDimension reps\n", argv[0]);
        printf("  mode one of { parfor1 parfor2 dnc cones }\n");
        exit(-1);
    }
    if (argc > 1)  version = argv[1];
    if (argc > 2)  dim     = atoi(argv[2]);
    if (argc > 3)  tiledim = atoi(argv[3]);
    if (argc > 4)  reps    = atoi(argv[4]);

    //printf("Constructing %dx%d matrix of tiles of dimension %dx%d..\n", dim, dim, tiledim, tiledim);

    // ----------------------------------------
    // This completely fails to call the ivar constructor.
    // for(int i=0; i< dim*dim; i++) matrix.push_back( ivar<tile*>() );
    // matrix.reserve(dim*dim);

    // ----------------------------------------
    // This one calls the constructor ONCE, and then copy constructors:
    matrix = new vector< myvar<tile*> >( dim*dim );

    // ----------------------------------------
    // This version works:
    vector< myvar<tile*>* > bar(dim*dim);
    // for(int i=0; i< dim*dim; i++) bar[i] = new ivar<tile*>();

    //printf("Done constructing matrix.\n");

    //(*matrix)[0].put( new tile() );
    //printf("Filled in initial tile.\n");

    if(! strcmp("parfor1",version) ) {// cilk_spawn
     // warm things up
//      traverse1();
 //     cilk_sync;

      TIMER_START(t);
      start = getticks(); 
      traverse1();
      end = getticks();
      TIMER_STOP(t);

    } else if(! strcmp("parfor2",version) ) { // cilk_spawn
      // warm things up
  //    traverse2();
   //   cilk_sync;
     
      TIMER_START(t);
        start = getticks(); 
        traverse2();
        end = getticks();
        TIMER_STOP(t);

      }else if(! strcmp("cones",version) ) {// cilk_spawn
      // warm things up
    //    spawn_cones();
     //   cilk_sync;
        TIMER_START(t);
        start = getticks(); 
        spawn_cones();
        end = getticks();
        TIMER_STOP(t);

      }else if(! strcmp("cones2",version) ) {
      //  spawn_cones2();
       // cilk_sync;
        
        TIMER_START(t);
        start = getticks(); 
        spawn_cones2();
        end = getticks();
        TIMER_STOP(t);

      } else if(! strcmp("dnc",version) ) {
        // Invariant: you will never see a BLOCK on ivar in this version:
        if(isPowerOfTwo(dim)){
          // warm up
        //  divide_and_conquer(0,0, dim, dim);
         // cilk_sync;

          TIMER_START(t);
          start = getticks(); 
          divide_and_conquer(0,0, dim, dim);
          end = getticks();
          TIMER_STOP(t);
        } else {
          printf("number need to be a power of two for dnc!\n");
        }
    } else {
      printf("UNRECOGNIZED TRAVERSAL STRATEGY: %s\n", version);
      abort();
    }

    //printf("Beginning to consume matrix results...\n");
    unsigned long long sum = sum_matrix();
   //printf("Read all positions, summed to: %lu\n", sum);

    //printf("Done with matrix test.  Successfull.\n");
    printf("%d\t%f\t%lf\n", dim, TIMER_EVAL(t), elapsed(end,start));
    return 0;
}

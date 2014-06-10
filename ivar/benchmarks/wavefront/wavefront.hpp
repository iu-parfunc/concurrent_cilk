#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <cilk/cilk.h>
#include <cilk/ivar.h>
#include <cilk/cilk_api.h>

#define IS_DEBUG 1
#define dbgprintf(str, ...) if (IS_DEBUG) { printf(str, __VA_ARGS__); }

using namespace std;

class tile {
  public: 

    tile() : vec(dim*dim), dim(2) {
      for(int i=0; i<dim*dim; i++) vec[i] = 1;
    }

    tile(int my_dim) : vec(my_dim*my_dim), dim(my_dim) {
      for(int i=0; i<dim*dim; i++) vec[i] = 1;
    }

    int sum() {
      int sum = 0;
      for(int i=0; i< dim * dim; i++) { sum += vec[i]; }
      return sum;
    }
    tile &operator+=(tile &t) {
      return *this;
    }

    int getDim() { return dim; }
    vector<int> getVec() { return vec; }
  private:
    vector<int> vec;
    int dim;

};

tile operator+(tile &t1, tile &t2) {
  int dim = t1.getDim();
  if(! (t1.getDim() == t2.getDim())) { abort(); }
  for(int i=0; i < (dim * dim); i++) {
    t1.getVec()[i] + t2.getVec()[i];
  }
  return t1;
}


class matrix {
  public:
    matrix() : dim(2), mat(new vector< ivar<tile*> >(dim*dim)) {}
    matrix(int my_dim) :  dim(my_dim), mat(new vector< ivar<tile*> >(my_dim*my_dim)) {}

    tile *get(int i, int j) {
      ivar<tile*> tmp = ((*mat))[i * dim + j];
      dbgprintf("READ ivar struct %p\n", *tmp.get_underlying_ivar());
      return tmp.get();
    }

    void put(int i, int j, tile* val) {
      ivar <tile*> tmp = (*mat)[i * dim + j];
      dbgprintf("WRITE putting value 0x%d at (%i,%i) ivar: %p\n", val, i,j, *tmp.get_underlying_ivar());
      tmp.put(val);
    }

    unsigned long long sum() {
      unsigned long long sum = 0;
      for(int i=0; i< dim*dim; i++) {
        sum += (*mat)[i].get()->sum();
      }
      return sum;
    }

    void do_tile(int i, int j) {
      tile *t = new tile();

      dbgprintf("put ivar at coordinate (%i,%i)\n", i, j);
      this->put(i,j, t);

      //if possible, add the tile diagonally above to the current tile.
      if(i>0 && j>0) { *t += *(this->get(i-1, j-1)); }

      //if possible add the tile above the current tile.
      if (i>0) { *t += *(this->get(i-1, j)); } 

      //if possible add the tile to the left of the current tile
      if (j>0) { *t += *(this->get(i, j-1)); }

    }

    //rows and column elements are computed in parallel.
    void traverse_by_par_row_and_column() {
      for (int i=0; i < dim; i++)  {
        for (int j=0; j < dim; j++)  {
          dbgprintf("spawning tile @(%i,%i)\n", i, j);
          cilk_spawn this->do_tile(i,j);
        }
      }
    }

    //rows are computed in parallel.
    void traverse_by_par_row() {
      for (int i=0; i < dim; i++) {
        cilk_spawn do_row(i);
      }
    }

    void traverse_by_dnc(int i, int j, int w, int h) {
      int mid_row, mid_col; 

      if (w == 1 && h == 1) {
        this->do_tile(i,j); 
      } else if (w == 1 || h == 1) { 
        abort(); 
      } else {
        mid_row = i + w/2;
        mid_col = j + h/2;
        cilk_spawn this->traverse_by_dnc(i, j, w/2, h/2);             // nw
        cilk_spawn this->traverse_by_dnc(mid_row, j, w/2, h/2);       // ne
        cilk_spawn this->traverse_by_dnc(i ,mid_col, w/2,h/2);        // sw
        cilk_spawn this->traverse_by_dnc(mid_row ,mid_col, w/2, h/2); // se 
      }
    }


  private:
    int dim;
    vector< ivar<tile*> > * mat;
    void do_row(int i) {
      for (int j=0; j < dim; j++) 
        this->do_tile(i,j);
    }
};




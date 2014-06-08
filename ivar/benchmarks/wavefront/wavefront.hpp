
class tile {
  public: 

    tile() : dim(2) vec(dim*dim) {
      for(int i=0; i<dim*dim; i++) vec[i] = 1;
    }

    tile(int my_dim) : dim(my_dim), vec(my_dim*my_dim) {
      for(int i=0; i<dim*dim; i++) vec[i] = 1;
    }

    int sum() {
      int sum;
      for(int i=0; i< dim * im; i++) { sum += vec[i]; }
      return sum;
    }

     tile &operator+=(const tile *tyle) {
      if(! dest.getDim() == dim) { abort("tiles are not the same size\n"); }
      for(int i=0; i < (dim * dim); i++) {
        vec[i] += tyle.getVec()[i];
      }
      return *this;
    }

    int getDim() { return dim; }
    vector<int> getVec() { return vec; }
  private:
    vector<int> vec;
    int dim;

};

class matrix {
  public:
    matrix() : dim(2), matrix(new vector<ivar<int*>>(dim*dim)) {}
    matrix(int my_dim) :  dim(my_dim), matrix(new vector<ivar<int*>>(my_dim*my_dim)) {}

    tile *get(int i, int j) {
      ivar<tile*> tmp = matrix[i * dim + j];
      return tmp.get();
    }

    void put(int i, int j, tile* val) {
      matrix[i * dim + j].put(val);
    }

    unsigned long long sum() {
      unsigned long long sum = 0;
      for(int i=0; i< dim*dim; i++) {
        sum += matrix[i].get().sum();
      }
      return sum;
    }

    void do_tile(int i, int j) {
      tile *t = new tile();

      //if possible, add the tile diagonally above to the current tile.
      if(i>0 && j>0) { t += matrix.get(i-1, j-1); }

      //if possible add the tile above the current tile.
      if (i>0) { t += matrix.get(i-1, j); } 

      //if possible add the tile to the left of the current tile
      if (j>0) { t += matrix.get(, j-1); }

      matrix.put(i,j, t);
    }

    void traverse_by_row() {
      for (int i=0; i < dim; i++)  {
        for (int j=0; j < dim; j++)  {
          cilk_spawn this.do_tile(i,j);
        }
      }
    }


  private:
    int dim;
    vector<ivar<tile*>> matrix;
};




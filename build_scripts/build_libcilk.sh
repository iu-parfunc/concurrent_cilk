#!/bin/bash


cd $CILK_SRC

rm -rf $CILK_ROOT
rm -rf build
mkdir build 
cd build 

cmake \
  -DCONCURRENT_CILK=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_INSTALL_PREFIX=$CILK_ROOT ..;

make && make install

cd ..


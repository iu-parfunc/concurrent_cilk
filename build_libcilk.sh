#!/bin/bash

rm -rf $CILK_ROOT
rm -rf build
mkdir build 
cd build 

CMAKE_C_COMPILER=icc CMAKE_CXX_COMPILER=icc cmake \
  -DCONCURRENT_CILK=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_INSTALL_PREFIX=$CILK_ROOT ..;

make && make install

cd ..


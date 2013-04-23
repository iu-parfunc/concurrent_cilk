#!/bin/bash


rm -rf build
mkdir build 
cd build 
DIR_BIN=.

cmake \
  -DCONCURRENT_CILK=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_INSTALL_PREFIX=$DIR_BIN ..;

make && make install

cd ..


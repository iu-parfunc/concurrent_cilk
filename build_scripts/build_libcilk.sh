#!/bin/bash

set -e

cd $CILK_SRC

rm -rf $CILK_ROOT
rm -rf build
mkdir build 
cd build 

echo "Where is cmake coming from? Here: "`which -a cmake`

#  -DCILK_DEBUG_IVARS=true \
cmake \
  -DCONCURRENT_CILK=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_INSTALL_PREFIX=$CILK_ROOT ..;
echo "CMake return code: echo $?"

make 
echo "Called generated makefile, return code: $?"
make install

cd ..


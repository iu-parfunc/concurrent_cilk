#!/bin/bash


[ -z "$CILK_ROOT" ] && echo "Need to set CILK_ROOT environment variable" && exit 1;
[ -z "$CILK_SRC" ] && echo "Need to set CILK_SRC environment variable" && exit 1;

#need to remove the old library or it doesn't copy? so weird
echo "removing old libraries..."
rm -f $CILK_ROOT/lib/libcilkrts.*
echo "success! building libcilkrts..."


pushd .

cd $CILK_SRC

rm -rf build
mkdir build 
cd build 

#-DCMAKE_BUILD_TYPE=Debug \
#-DCMAKE_BUILD_TYPE=Release \
#-DCMAKE_BUILD_TYPE=RelWithDebInfo \
cmake \
  -DCONCURRENT_CILK=ON \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_INSTALL_PREFIX=$CILK_ROOT ..;

make && make install

popd


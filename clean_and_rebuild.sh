#!/bin/bash

[ -z "$CILK_ROOT" ] && echo "Need to set CILK_ROOT environment variable" && exit 1;
[ -z "$CILK_SRC" ] && echo "Need to set CILK_SRC environment variable" && exit 1;

#need to remove the old library or it doesn't copy? so weird
echo "removing old libraries..."
rm -f $CILK_ROOT/lib/libcilkrts.*
echo "success! building libcilkrts..."

#build libcilk
make -C $CILK_SRC clean; make -j -C $CILK_SRC/Makefile; make -C $CILK_SRC install

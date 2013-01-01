#!/bin/bash

make -C $CILK_SRC clean; make -j -C $CILK_SRC/Makefile; make -C $CILK_SRC install

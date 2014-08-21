#!/bin/bash

set -xe

source build_scripts/set_compiler
source build_scripts/env_cilk
export PATH=$HOME/cmake-2.8.10/bin/cmake:$PATH
export CILK_IVAR_DEBUG=false

make

python cilk_tests/test_cilk.py 
# 2> /dev/null

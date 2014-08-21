#!/bin/bash

set -xe

source build_scripts/set_compiler
source build_scripts/env_cilk
export CILK_IVAR_DEBUG=false
git submodule init
git submodule update
make
python cilk_tests/test_cilk.py 2> /dev/null

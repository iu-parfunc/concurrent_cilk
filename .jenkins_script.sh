#!/bin/bash

source .jenkins_common_setup.sh

make clean
make
python cilk_tests/test_cilk.py 2> /dev/null

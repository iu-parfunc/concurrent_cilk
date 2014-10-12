#!/bin/bash

set -e

echo "Running .jenkins_script.sh in directory `pwd`"

# # Optional convention of passing the working directory as first arg:
# if [ -d $1]; then
#   cd $1
# fi

source .jenkins_common_setup.sh

make clean
make
python cilk_tests/test_cilk.py 2> /dev/null

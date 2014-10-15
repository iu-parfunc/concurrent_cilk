#!/bin/bash

set -xe

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"
echo "Script called with args: $*"
echo "Run on machine $HOSTNAME ... SHELL=$SHELL LANG=$LANG"

rootdir=$1

if [ "$rootdir" == "" ]; then
  echo "Error expected working dir as first arg"
  exit 1
else
  shift
fi

echo "Full locale is:"
locale || echo ok

export BENCHARGS=$*
echo "Read rootdir from first argument: $rootdir"

if ! [ -d "$rootdir" ]; 
then echo ".run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1 
fi 

cd $rootdir
echo "Switched to working-copy directory: "`pwd`
source .jenkins_common_setup.sh

# (1) Build the library
echo "Begin building at time: "`date`
make clean
make all

# (2) Then benchmark:
echo "Finished building libs, begin benchmarking, time: "`date`
make bench 

echo "Finished benchmarking at time: "`date`

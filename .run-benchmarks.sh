#!/bin/bash

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"
rootdir=$1
shift
export BENCHARGS=$*

if [ "$rootdir" == "" ] || ! [ -d "$rootdir" ]; 
then echo ".run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1 
fi 

source .jenkins_common_setup.sh

cd $rootdir
echo "Switched to working-copy directory: "`pwd`

# (1) Build the library
make clean
make all

# (2) Then benchmark:
make bench 

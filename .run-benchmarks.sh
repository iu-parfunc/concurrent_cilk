#!/bin/bash

set -e

# Script used by Jenkins to run benchmarks.

echo "Run benchmarks script starting, located at: $0"
echo "Run on machine $HOSTNAME ..."
rootdir=$1
shift
export BENCHARGS=$*
echo "Read rootdir from first argument: $rootdir"

if [ "$rootdir" == "" ] || ! [ -d "$rootdir" ]; 
then echo ".run-benchmarks, cannot proceed because rootdir ($rootdir) does not exist."
     exit 1 
fi 

cd $rootdir
echo "Switched to working-copy directory: "`pwd`

source .jenkins_common_setup.sh

# (1) Build the library
make clean
make all

# (2) Then benchmark:
make bench 

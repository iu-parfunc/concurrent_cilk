BIN=cilksort_inplace.exe 
# cilksort_outofplace.exe
# cilksort_ivars.exe

default: all simple

OUT=bin
SCRIPTS="../../scripts"
SCRIPT_PREFIX="../benchmarks/sorting"
include  ../../Makefile.common

simple: 
	icc -lcilkrts cilksort_inplace.c -o cilksort_inplace_plainicc.exe
#	icc -lm -lcilkrts cilksort_inplace.c -o cilksort_inplace_icc.exe

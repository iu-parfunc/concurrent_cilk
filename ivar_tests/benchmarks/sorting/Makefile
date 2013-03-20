BIN= cilksort_inplace.exe cilksort_outofplace.exe cilksort_ivars.exe

default: all simple

OUT=bin
SCRIPTS="../../scripts"
SCRIPT_PREFIX="../benchmarks/sorting"
include  ../../Makefile.common

simple: cilksort_inplace_plainicc.exe cilksort_outofplace_plainicc.exe

cilksort_inplace_plainicc.exe: cilksort_inplace.c
	icc -O3 -lcilkrts $^ -o $@

cilksort_outofplace_plainicc.exe: cilksort_outofplace.c
	icc -O3 -lcilkrts $^ -o $@

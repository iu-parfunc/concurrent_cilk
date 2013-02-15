
#vars for the tests to compile. run them all with make test
BIN= microbench_many_blocking.exe \
  wavefront.exe  							 \
	microbench.exe               \

#directory where compiled files wil be put
OUT=bin

#path to run_tests.sh
SCRIPTS="../scripts"

#path from scripts to the current directory
SCRIPT_PREFIX="../benchmarks"

#magic
include ../Makefile.common

.PHONY: test
test: parfor1 parfor2 dnc 

.PHONY: parfor1
parfor1: $(addprefix $(OUT)/,wavefront.exe)
	$(OUT)/wavefront.exe parfor1 120 60

.PHONY: parfor2
parfor2: $(addprefix $(OUT)/,wavefront.exe)
	$(OUT)/wavefront.exe parfor2 120 60

.PHONY: dnc
dnc: $(addprefix $(OUT)/,wavefront.exe)
	$(OUT)/wavefront.exe dnc 128 64

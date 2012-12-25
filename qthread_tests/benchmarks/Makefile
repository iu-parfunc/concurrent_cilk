
#vars for the tests to compile. run them all with make test
BIN= qthread_microbench.exe \
		 hello_world.exe        \
		 test.exe 							\

#directory where compiled files wil be put
OUT=bin

#magic
include ../Makefile.common

.PHONY: qthread_microbench
qthread_microbench: $(addprefix $(OUT)/,qthread_microbench.exe)
	$(OUT)/qthread_microbench.exe 1000000

.PHONY: hello_world
hello_world: $(addprefix $(OUT)/,hello_world.exe)
	$(OUT)/hello_world.exe

.PHONY: test
test: $(addprefix $(OUT)/,test.exe)
	$(OUT)/test.exe

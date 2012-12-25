TOP=/nobackup/parfunc/concurrent_cilk
BUILD=gcc-4.7-cilkrts
LIBS=$(TOP)/$(BUILD)/lib/:$(TOP)/$(BUILD)/lib64/:$(TOP)/$(BUILD)/libexec/
INCLUDE=$(TOP)/$(BUILD)/include/
CFLAGS= -lcilkrts -lpthread -ldl -g -ggdb -std=c99 -O3

GCC=$(TOP)/$(BUILD)/bin/gcc

parfib: parfib.c
	$(GCC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) parfib.c -o parfib

testcilk: testcilk.c
	$(GCC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) testcilk.c -o testcilk


run: parfib
	./parfib

clean:
	rm testcilk parfib

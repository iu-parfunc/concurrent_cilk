
#global properties related to the runtime location 
RTSDIR:=$(shell echo $(CILK_ROOT))
INCLUDEDIR=$(RTSDIR)/include/cilk
EXT=so

GCC=$(RTSDIR)/bin
CC=$(GCC)/gcc -L$(RTSDIR)/lib:$(RTSDIR)/lib64 -I$(INCLUDEDIR)
CPLUS=$(GCC)/g++ -L$(RTSDIR)/lib:$(RTSDIR)/lib64 -I$(INCLUDEDIR)

#lib dependencies
LIBDEPS= $(RTSDIR)/lib/libcilkrts.$(EXT)
LIBS=$(RTSDIR)/lib/:$(RTSDIR)/lib64/:$(RTSDIR)/libexec/

INCLUDE=$(RTSDIR)/include
CFLAGS+= -lcilkrts -lpthread -ldl -fcilkplus -std=c99 -g3 -ggdb -Wno-int-to-pointer-cast -O3 -I$(INCLUDE) -L$(LIBS)
CPLUSFLAGS+= -lcilkrts -lpthread -ldl -fcilkplus -std=c99 -g3 -ggdb -Wno-int-to-pointer-cast -fpermissive -O2 -I$(INCLUDE) -L$(LIBS) 


parfib: parfib.c
	$(CC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) parfib.c -o parfib.exe

testcilk: testcilk.c
	$(CC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) testcilk.c -o testcilk.exe


run: parfib
	./parfib

clean:
	rm -f *.o *.exe


#global properties related to the runtime location 
RTSDIR=/nobackup/czakian
BUILD=cilkplus
INCLUDEDIR=$(RTSDIR)/$(BUILD)/include/cilk
EXT=so

GCC=$(RTSDIR)/$(BUILD)/bin
CC=$(GCC)/gcc -L$(RTSDIR)/$(BUILD)/lib:$(RTSDIR)/$(BUILD)/lib64 -I$(INCLUDEDIR)
CPLUS=$(GCC)/g++ -L$(RTSDIR)/$(BUILD)/lib:$(RTSDIR)/$(BUILD)/lib64 -I$(INCLUDEDIR)

#lib dependencies
LIBDEPS= $(RTSDIR)/$(BUILD)/lib/libcilkrts.$(EXT)
LIBS=$(RTSDIR)/$(BUILD)/lib/:$(RTSDIR)/$(BUILD)/lib64/:$(RTSDIR)/$(BUILD)/libexec/

INCLUDE=$(RTSDIR)/$(BUILD)/include
CFLAGS+= -lcilkrts -lpthread -ldl -fcilkplus -g -ggdb -Wno-int-to-pointer-cast -O3 -I$(INCLUDE) -L$(LIBS)
CPLUSFLAGS+= -lcilkrts -lpthread -ldl -fcilkplus -g -ggdb -Wno-int-to-pointer-cast -fpermissive -O2 -I$(INCLUDE) -L$(LIBS) 


parfib: parfib.c
	$(CC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) parfib.c -o parfib.exe

testcilk: testcilk.c
	$(CC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) testcilk.c -o testcilk.exe


run: parfib
	./parfib

clean:
	rm -f *.o *.exe

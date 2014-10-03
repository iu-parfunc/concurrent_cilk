
ARGS= -g -Wall
INCLUDES="/u/budkahaw/Builds/cilk/libevent/libevent-2.0.21-stable/include"
LDFLAGS  = 
# CC  = gcc $(ARGS)
# CPP = g++ -std=c++11 $(ARGS)
# CC  = clang $(ARGS)
# CPP = clang $(ARGS)
CC  = icc $(ARGS) 
CPP = icpc -std=c++11 $(ARGS) 
AR = ar
RM = rm

HDRS = 
CODE = cc.c
OBJ0 = $(CODE:.cpp=.o)
OBJ  = $(OBJ0:.c=.o)
SRC = $(HDRS) $(CODE)
LIBS = -L/u/budkahaw/Builds/cilk/libevent_2.1/libevent-2.1.4-alpha/bin/lib -levent -lpthread 
CFLAGS += -I/u/budkahaw/Builds/cilk/libevent_2.1/libevent-2.1.4-alpha/bin/include

.PHONY: all build install 

all: build install

build:	$(OBJ)
	$(CC) $(CFLAGS) $(LIBS) concurrent_cilk_io.o -o server.exe

run:

.cpp.o:
	$(CPP) $(CFLAGS) $(OPTS) -c -Wall -o $@ $<
.c.o:
	$(CC) $(CFLAGS) $(OPTS) -c -Wall -o $@ $<

clean:
	$(RM) -f $(OBJ) *.exe *.a *.so

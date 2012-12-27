CFLAGS= -lcilkrts -lpthread -ldl -fcilkplus -g -ggdb -std=c99 -O3
GCC=gcc

parfib: parfib.c
	$(GCC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) parfib.c -o parfib.exe

testcilk: testcilk.c
	$(GCC) $(CFLAGS) -I$(INCLUDE) -L$(LIBS) testcilk.c -o testcilk.exe


run: parfib
	./parfib

clean:
	rm -f *.o *.exe

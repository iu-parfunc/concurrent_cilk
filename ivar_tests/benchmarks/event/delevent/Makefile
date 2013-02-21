include ../../../Makefile.common

OUT=bin
OBJS=../event_cilk.o ../lockfree_queue.o main.o

$(OUT)/%.exe: $(OBJS)
	mkdir -p $(OUT)
	$(CC) $(OBJS) -o $@ $(LIBDEPS) $(OPTS) $(CFLAGS)

.PHONY: clean
clean:
	rm -f $(OUT)/*.exe
	rm -f $(OBJS)
	rmdir $(OUT)


include ../../../Makefile.common

OUT=bin
OBJS=../event_cilk.o ../lockfree_queue.o add.o #main.o

$(OUT)/%.exe: $(OBJS)
	mkdir -p $(OUT)
	$(CC) $(OBJS) -o $@ $(LIBDEPS) $(OPTS) $(CFLAGS)

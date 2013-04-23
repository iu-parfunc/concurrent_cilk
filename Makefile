
all: rebuild

build:
	./build_scripts/build_libcilk.sh

rebuild:
	./build_scripts/clean_and_rebuild.sh

clean:
	rm -rf build

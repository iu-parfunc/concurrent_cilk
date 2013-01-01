#!/bin/bash

libtoolize; aclocal; automake --add-missing; autoconf -i; 

echo "build is setup. please run ./configure --prefix=<path-to-gcc-build, then make; make install>"
  


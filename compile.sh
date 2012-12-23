#!/bin/bash
./configure --prefix=/opt/libcilk; libtoolize; aclocal; automake --add-missing; autoconf -i; make; make install


#!/bin/bash
./configure --prefix=/opt/cilkplus; libtoolize; aclocal; automake --add-missing; autoconf -i; make; make install


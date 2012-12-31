#!/bin/bash
./configure --prefix=/nobackup/czakian/cilkplus; libtoolize; aclocal; automake --add-missing; autoconf -i; make; make install


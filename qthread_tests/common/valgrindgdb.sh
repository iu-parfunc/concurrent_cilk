#!/bin/bash
#valgrind --tool=memcheck --trace-signals=yes --show-reachable=yes --vex-iropt-precise-memory-exns=yes. --db-attach=yes $1
valgrind --trace-signals=yes --show-reachable=yes --vex-iropt-precise-memory-exns=yes.  $1

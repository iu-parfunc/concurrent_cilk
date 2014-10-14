# Sourced from the other bash scripts in this directory.

set -x
set -e
source build_scripts/set_up_cilk_iu
source build_scripts/env_cilk
export CILK_IVAR_DEBUG=false

echo "Using compiler, CONCURRENTCILK_CC = $CONCURRENTCILK_CC .  Version info:"
$CONCURRENTCILK_CC --version
$CONCURRENTCILK_CXX --version

export CONCURRENTCILK_CC
export CONCURRENTCILK_CXX

# This happens in the jenkins web-based script:
# git submodule init --update --recursive

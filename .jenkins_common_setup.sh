# Sourced from the other bash scripts in this directory.

set -xe
source build_scripts/set_compiler
source build_scripts/env_cilk
export CILK_IVAR_DEBUG=false
git submodule init
git submodule update

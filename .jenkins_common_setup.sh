# Sourced from the other bash scripts in this directory.

set -xe
source build_scripts/set_compiler
source build_scripts/env_cilk
export CILK_IVAR_DEBUG=false

# This happens in the jenkins web-based script:
# git submodule init --update --recursive

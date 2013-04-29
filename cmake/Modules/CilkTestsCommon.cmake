
###############################################################################
# Build for Cilk Plus runtime library
###############################################################################

#set 2.8 as the minimum version
cmake_minimum_required(VERSION 2.8.8)

# Only uncomment one at a time. To date (April 21, 2013)
# These are all the compilers that support cilkplus
# -----------------------------------------

# Set clang/clang++ as the compiler
#set(CMAKE_C_COMPILER "clang")
#set(CMAKE_CXX_COMPILER "clang++")

# Set gcc as the compiler
#set(CMAKE_C_COMPILER "gcc")
#set(CMAKE_CXX_COMPILER "g++")

# Set icc as the compiler
set(CMAKE_C_COMPILER "icc")
set(CMAKE_CXX_COMPILER "icc")

# -----------------------------------------

#put the common folder at top level of cilk_tests onto the include path
set(COMMON "$ENV{CILK_SRC}/cilk_tests/common")
include_directories(${COMMON})


#put our compiled libcilkrts on the path
#if you want a different libcilk, use:
#LD_PRELOAD=<path to your own libcilkrts> ./<your_executable>.exe
set(LIB_SEARCH_PATH "$ENV{CILK_ROOT}/lib")
set(INCLUDE_SEARCH_PATH "$ENV{CILK_ROOT}/include")
set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH} ${INCLUDE_SEARCH_PATH})
set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${LIB_SEARCH_PATH})

# As part of cmake, one can set the build type to be Release, Debug, Release with Debug, etc. 
# see http://www.cmake.org/Wiki/CMake_Useful_Variables
# these definitions give us a hook into compile flags for 
# -----------------------------------------

# These flags are only add in Debug build type. 
# Tell gdb we really want all the info.
set(EXTRA_DEBUG_FLAGS "-ggdb -g3 -fvar-tracking-assignments")

# base flags for C/C++ that are used irrespective of build type
set(BASE_C_FLAGS "-Wall -std=c99")
set(BASE_CXX_FLAGS "-Wall")


#C Additions
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${BASE_C_FLAGS}")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${BASE_C_FLAGS}")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} ${BASE_C_FLAGS} ${EXTRA_DEBUG_FLAGS}")
set(CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} ${BASE_C_FLAGS}")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${BASE_C_FLAGS} ${EXTRA_DEBUG_FLAGS}")

#C++ Additions
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${BASE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${BASE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${BASE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} ${BASE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${BASE_CXX_FLAGS} ${EXTRA_DEBUG_FLAGS}")

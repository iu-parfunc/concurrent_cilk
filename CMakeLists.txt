###############################################################################
# Build for Cilk Plus runtime library
###############################################################################

#set 2.8 as the minimum version
cmake_minimum_required(VERSION 2.8.8)

# Only uncomment one at a time. To date (April 21, 2013)
# These are all the compilers that support cilkplus
# -----------------------------------------

# Set clang/clang++ as the compiler
set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")

# Set gcc as the compiler
#set(CMAKE_C_COMPILER "gcc")
#set(CMAKE_CXX_COMPILER "g++")

# Set icc as the compiler
#set(CMAKE_C_COMPILER "icc")
#set(CMAKE_CXX_COMPILER "icc")

# -----------------------------------------

#turn on concurrent cilk by default. (everything for CCC conditionally included on this variable)
option(CONCURRENT_CILK ON)

# If there is an extra file that needs to get compiled, add it here. 
# -----------------------------------------

# Header files
set(cilk_header_files
  cilk/cilk.h
  cilk/cilk_api.h
  cilk/cilk_api_linux.h
  cilk/cilk_stub.h
  cilk/cilk_undocumented.h
  cilk/common.h
  cilk/holder.h
  cilk/hyperobject_base.h
  cilk/reducer.h
  cilk/reducer_file.h
  cilk/reducer_list.h
  cilk/reducer_max.h
  cilk/reducer_min.h
  cilk/reducer_opadd.h
  cilk/reducer_opand.h
  cilk/reducer_opor.h
  cilk/reducer_opxor.h
  cilk/reducer_ostream.h
  cilk/reducer_string.h
  )

# Conditionally append concurrent cilk headers
if(${CONCURRENT_CILK})
  set(cilk_header_files 
    ${cilk_header_files} 
    internal/abi.h
    cilk/ivar.h
    cilk/concurrent_cilk.h
    cilk/concurrent_queue.h
    )
  endif()

  # Sources files
  set(cilk_runtime_sources
    runtime/cilk-abi.c
    runtime/cilk_api.c
    runtime/cilk_malloc.c
    runtime/c_reducers.c
    runtime/frame_malloc.c
    runtime/full_frame.c
    runtime/jmpbuf.c
    runtime/local_state.c
    runtime/metacall_impl.c
    runtime/os_mutex-unix.c
    runtime/os-unix.c
    runtime/scheduler.c
    runtime/signal_node.c
    runtime/stacks.c
    runtime/stats.c
    runtime/symbol_test.c
    runtime/sysdep-unix.c
    runtime/worker_mutex.c
    runtime/bug.cpp
    runtime/cilk-abi-cilk-for.cpp
    runtime/except-gcc.cpp
    runtime/global_state.cpp
    runtime/reducer_impl.cpp
    runtime/pedigrees.c
    )

  # Conditionally append concurrent cilk sources
  if(${CONCURRENT_CILK})
    set(cilk_runtime_sources
      ${cilk_runtime_sources} 
      runtime/concurrent_cilk.c
      runtime/ivar_full_blocking.c
      runtime/concurrent_queue.c
      )
    endif()


    # -----------------------------------------

    # point cmake to the right place for the output directory
    # that will contain the header files
    set(output_dir ${CMAKE_INSTALL_PREFIX}/include)

    # Install cilk header files
    set(out_files)
    foreach( f ${cilk_header_files} )
      set( src ${CMAKE_CURRENT_SOURCE_DIR}/include/${f} )
      set( dst ${output_dir}/${f} )
      add_custom_command(OUTPUT ${dst}
        DEPENDS ${src}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${src} ${dst}
        COMMENT "Copying cilk's ${f}...")
      list(APPEND out_files ${dst})
    endforeach( f )
    add_custom_target(cilk-headers ALL DEPENDS ${out_files})
    set_target_properties(cilk-headers PROPERTIES FOLDER "Misc")


    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/runtime)


    # As part of cmake, one can set the build type to be Release, Debug, Release with Debug, etc. 
    # see http://www.cmake.org/Wiki/CMake_Useful_Variables
    # these definitions give us a hook into compile flags for 
    # -----------------------------------------

    # These flags are only add in Debug build type. 
    # Tell gdb we really want all the info.
    set(EXTRA_DEBUG_FLAGS "-ggdb -g3 -fvar-tracking-assignments")

    # base flags for C/C++ that are used irrespective of build type
    set(BASE_C_FLAGS "-Wall -std=c11 -Wno-unused-value -DIVAR_DBG=0xf")
    set(BASE_CXX_FLAGS "-Wall -Wno-unused-value")

    if(${CMAKE_C_COMPILER} MATCHES "clang")
      set(BASE_C_FLAGS "-fcilkplus ${BASE_C_FLAGS}")
      set(BASE_CXX_FLAGS "-fcilkplus ${BASE_CXX_FLAGS}")
    endif()


    #C Additions
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${BASE_C_FLAGS}")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} ${BASE_C_FLAGS}")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} ${BASE_C_FLAGS}")
    set(CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} ${BASE_C_FLAGS}")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${BASE_C_FLAGS} ${EXTRA_DEBUG_FLAGS}")

    #C++ Additions
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${BASE_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${BASE_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${BASE_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} ${BASE_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${BASE_CXX_FLAGS} ${EXTRA_DEBUG_FLAGS}")

    # -----------------------------------------

    #default definitions
    add_definitions(
      -D__CILKRTS_ABI_VERSION=1                    # ABI is 1 by default
      -DIN_CILK_RUNTIME=1
      -DBUILD_USER="anonymous"                     # No user variable in CMAKE
      -DBUILD_HOST="${CMAKE_HOST_SYSTEM_NAME}"     # this is a predefined CMAKE variable
      )

    if(${CONCURRENT_CILK})
      message("-- CMake setup with Concurrent Cilk ON")
      add_definitions(-DCONCURRENT_CILK)
    endif()


    set(CILKRTS cilkrts)
    add_library(${CILKRTS} SHARED ${cilk_runtime_sources})
    install(TARGETS
      ${CILKRTS} 
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      LIBRARY
      DESTINATION lib
      )

    #apparently you need soversion AND version in order to make this not error...
    #cmake says nothing on the sort in its documentation...
    set_target_properties(${CILKRTS}
      PROPERTIES
      SOVERSION 5
      VERSION 0.0.5
      )


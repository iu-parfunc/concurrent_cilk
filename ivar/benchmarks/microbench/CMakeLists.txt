# -----------------------------------------
cmake_minimum_required(VERSION 2.8.8)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "$ENV{CILK_SRC}/cilk_tests/cmake/Modules/")

include(CilkTestsCommon)
include(TargetDistclean)

#get all of the c and cpp files in the current directory
file(GLOB SRC 
  "*.c"
  "*.cpp")


#for each source file, make it an executable filename and add it as an executable target
foreach(f ${SRC})
  string(REGEX REPLACE ".cpp$|.c$" ".exe" loc ${f})
  get_filename_component(exec ${loc} NAME)
  add_executable(${exec} ${f})
  target_link_libraries(${exec} dl pthread cilkrts)
  list(APPEND executables "build/${exec}")
endforeach(f)

install(PROGRAMS ${executables} DESTINATION "${CMAKE_SOURCE_DIR}/bin")




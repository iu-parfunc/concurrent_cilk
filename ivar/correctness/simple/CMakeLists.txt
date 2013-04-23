# -----------------------------------------
cmake_minimum_required(VERSION 2.8.8)

include(../../../cmake/Modules/CilkTestsCommon.cmake)

file(GLOB SRC 
  "*.c"
  "*.cpp")


foreach(f ${SRC})
  string(REGEX REPLACE ".c" ".exe" loc ${f})
  get_filename_component(exec ${loc} NAME)
  add_executable(${exec} ${f})
  target_link_libraries(${exec} dl pthread cilkrts)
  list(APPEND executables "build/${exec}")
endforeach(f)

install(PROGRAMS ${executables} DESTINATION bin)




# -----------------------------------------
cmake_minimum_required(VERSION 2.8.8)

include(../cmake/Modules/CilkTestsCommon.cmake)

#default definitions
add_definitions()

add_executable(parfib.exe parfib.c)
target_link_libraries(parfib.exe dl pthread cilkrts)

add_executable(ivars_parfib.exe ivars_parfib.c)
target_link_libraries(ivars_parfib.exe dl pthread cilkrts)

install(TARGETS parfib.exe ivars_parfib.exe RUNTIME DESTINATION bin)




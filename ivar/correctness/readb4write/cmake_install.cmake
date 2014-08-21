# Install script for directory: /home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "0")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/restore_out_of_order.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/fastpathonly.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/nested_deadlock_delay_writer.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/regular_cilk.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/pfib.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/spawnedrw.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/nested_deadlock.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/doublereadslowpath.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/nested_deadlock_assignment.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/minimal_cilk.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/tmp.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/snafoo.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/nreaders.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/delayed_writer_reverse_write.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/interleaved_read_write.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/delayed_writer.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/testcppapi.exe;/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin/testcppivartemplates.exe")
  IF (CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  ENDIF (CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
  IF (CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  ENDIF (CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
FILE(INSTALL DESTINATION "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/bin" TYPE PROGRAM FILES
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/restore_out_of_order.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/fastpathonly.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/nested_deadlock_delay_writer.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/regular_cilk.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/pfib.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/spawnedrw.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/nested_deadlock.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/doublereadslowpath.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/nested_deadlock_assignment.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/minimal_cilk.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/tmp.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/snafoo.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/nreaders.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/delayed_writer_reverse_write.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/interleaved_read_write.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/delayed_writer.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/testcppapi.exe"
    "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/build/testcppivartemplates.exe"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "/home/czakian/Research/concurrent_cilk/cilk_tests/ivar/correctness/readb4write/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)

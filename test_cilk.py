#! /usr/bin/env python 

# This script is run from CILK_ROOT
import os
import glob
import sys

failed = 0
failed_tests = []

failed_builds = 0
failed_build_dirs = []

for root, dirs, files in os.walk("./cilk_tests/"):
  # Don't go into .git directories
  # or the auto generated "build" dirs
  if (".git" not in root and "build" not in root and "pbbs" not in root):
    make_huh = False
    notest = False
    for file in files: 
      if file == "Makefile": # Have Makefile?
        make_huh = True
      if file == "NOTEST": # Should we test this directory?
        make_huh = False
        notest = True
        break
  if make_huh and not notest:
    cwd = os.getcwd() # get current directory
    try:
      os.chdir(root)
      print "BUILDING in dir: ", root
      code = os.system("make -B") # Remake everything
      if code != 0:
        failed_builds += 1
        failed_build_dirs.append(root)
      for exe in glob.glob("*.exe"):     # or in the current directory (we'll get rid of this eventually)
        print "Running test: ", exe
        ret_code = os.system("timeout 10s " + exe)
        if ret_code != 0:
          failed += 1
          failed_tests.append(exe)
      #for exe in glob.glob("bin/*.exe"): # we either installed in bin/
      #  ret_code = os.system("timeout 10s " + exe)
      #  if ret_code != 0:
      #    failed += 1
      #    failed_tests.append(exe)
    finally:
      os.chdir(cwd)

print "================== Tests Finished ========================"
print "Number of failed tests  : ", failed
print "Tests that failed       : ", failed_tests
print "Number of failed builds : ", failed_builds
print "Builds that failed      : ", failed_build_dirs
print "=========================================================="

# For now, don't report failed tests, but instead report failed builds
if failed_builds != 0:
  sys.exit(1)


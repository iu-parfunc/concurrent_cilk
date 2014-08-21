# This script is run from CILK_ROOT
import os
import glob

failed = 0
failed_tests = []

for root, dirs, files in os.walk("./cilk_tests/"):
  # Don't go into .git directories
  # or the auto generated "build" dirs
  if (".git" not in root and "build" not in root):
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
      os.system("make -B") # Remake everything
      for exe in glob.glob("*.exe"):     # or in the current directory (we'll get rid of this eventually)
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
print "Number of failed tests: ", failed
print "Tests that failed     : ", failed_tests
print "=========================================================="

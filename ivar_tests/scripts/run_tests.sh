#!/bin/bash

# No reason to do anything sophisticated here because if things
# progress further we should move tests into the proper ICL test
# suite.

# set -e 

FAILED=""
NUMFAILED=0

for test in "$@"; do 
  echo
  echo "Running test: $test"
  echo "========================================"
  $test
  CODE=$?  

  if [ "$CODE" == 0 ];
  then echo "Test exited successfully."
  else echo "ERROR: TEST FAILED"
       FAILED="$test $FAILED"
       NUMFAILED=$((NUMFAILED+1))
  fi
done

echo
echo
if [ "$NUMFAILED" == 0 ];
then echo "  run_tests.sh finished successfully.  Tests passed."
else echo "  <--- Encountered $NUMFAILED failures. --->"
     echo "FAILED TESTS:   $FAILED"
     exit 1
fi

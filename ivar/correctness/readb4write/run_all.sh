#!/bin/bash

function run_all() {
fail ="0"
success ="0"
failed_tests = "";
succeeded_tests ="";

for exe in $(ls *.exe); do
  ./$exe;
  CODE=$?;
  if [ "$CODE" != 0 ] && [ "$CODE" != 21 ];
  then
    echo Command returned error code: $CODE;
    fail=$(($fail+1));
    failed_tests="$failed_tests $exe"
  else
    success=$(($success+1));
    succeeded_tests="$succeeded_tests $exe"
  fi
done

total = $(($fail+$success));
printf "\n ------------------------------------------------------------------\n"
printf "                     TESTS DONE                                      \n"
printf " ------------------------------------------------------------------\n"
printf "Total: %i\nSucceeded: %i\nFailed: %i\n"  "$total" "$success" "$fail";
printf "Failed Tests: %s\n" "$failed_tests";
printf "Succeeded Tests: %s\n" $succeeded_tests;
}

run_all

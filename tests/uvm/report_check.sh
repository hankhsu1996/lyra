#!/bin/sh
# Judges a finished UVM run as the Makefiles UVM ships with its examples judge
# one: the report server's summary in the run's output says exactly the
# expected count of errors and of fatals, each once. Run from the directory the
# run started in, with the two counts as its arguments.
errors=$1
fatals=$2
log=_execute/stdout.log
test "$(grep -c "UVM_ERROR :    $errors" "$log")" -eq 1 &&
  test "$(grep -c "UVM_FATAL :    $fatals" "$log")" -eq 1

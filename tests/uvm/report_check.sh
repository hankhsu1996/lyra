#!/bin/sh
# Judges a finished UVM run as the Makefiles UVM ships with its examples judge
# one: the report server's summary in the run's output says exactly the
# expected count of errors and of fatals, each once. Run from the directory the
# run started in, with the two counts as its arguments. A run judged otherwise
# has its first error or fatal report and the summary's counts printed, which
# is what the record of a case stopping here quotes.
errors=$1
fatals=$2
log=_execute/stdout.log
if test "$(grep -c "UVM_ERROR :    $errors" "$log")" -eq 1 &&
  test "$(grep -c "UVM_FATAL :    $fatals" "$log")" -eq 1; then
  exit 0
fi
grep -m 1 -E '^UVM_(ERROR|FATAL) [^:]' "$log"
grep -E '^UVM_(ERROR|FATAL) :' "$log"
exit 1

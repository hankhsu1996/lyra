// process::self() returns a handle to the process making the call, so two
// calls from one process name the same process, and a process asking about
// itself while executing is RUNNING -- the state of a process that is running
// rather than waiting in a blocking statement (LRM 9.7). Each parallel
// statement of a fork-join block is a process of its own (LRM 9.5), so a
// branch calling self() gets a handle to itself and not to its parent. A
// variable's declaration assignment runs before any procedure starts (LRM
// 10.5), so no process makes a call written there, and the handle it answers
// names none: it is null.
module Top;
  int running_self, same_handle, distinct_handles;
  bit initializer_ran;

  process outer;
  process branch;

  function automatic process self_from_an_initializer();
    initializer_ran = 1;
    return process::self();
  endfunction

  process from_initializer = self_from_an_initializer();

  initial begin
    outer = process::self();
    running_self = (outer.status() == process::RUNNING);
    same_handle = (process::self() == outer);
    fork
      branch = process::self();
    join
    distinct_handles = (branch != outer);
  end

  final begin
    if (running_self !== 1)
      $fatal(1, "running_self was %0d, expected 1", running_self);
    if (same_handle !== 1)
      $fatal(1, "same_handle was %0d, expected 1", same_handle);
    if (distinct_handles !== 1)
      $fatal(1, "distinct_handles was %0d, expected 1", distinct_handles);
    if (initializer_ran !== 1)
      $fatal(1, "the declaration assignment never ran");
    if (from_initializer != null)
      $fatal(1, "process::self() in a declaration assignment was not null");
    $display("All checks passed");
  end
endmodule

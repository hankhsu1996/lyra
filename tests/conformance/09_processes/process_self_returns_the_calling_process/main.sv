// process::self() returns a handle to the process making the call, so two
// calls from one process name the same process, and a process asking about
// itself while executing is RUNNING -- the state of a process that is running
// rather than waiting in a blocking statement (LRM 9.7). Each parallel
// statement of a fork-join block is a process of its own (LRM 9.5), so a
// branch calling self() gets a handle to itself and not to its parent.
module Top;
  int running_self, same_handle, distinct_handles;

  process outer;
  process branch;

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
    $display("All checks passed");
  end
endmodule

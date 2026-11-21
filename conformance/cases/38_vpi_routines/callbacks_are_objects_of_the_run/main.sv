// A callback is an object of the simulation (LRM 37.80): its type is
// `vpiCallback`, `vpi_get_cb_info` reads back what it was registered with
// (LRM 38.8), an object relates to the callbacks placed on it and no object to
// those related to none of the diagram's objects -- one placed on a task among
// them, a task being none of them -- and a time queue to the callbacks it
// holds. The time queues are objects too, iterated in increasing time, each
// answering its time (LRM 37.81). A removed callback is never
// called, keeps no time queue, and its handle is no longer valid (LRM 38.39),
// while releasing a callback's handle leaves the callback (LRM 38.38). A
// callback that removes itself still has the object it was handed until its
// call returns (LRM 38.36).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int inspect();
  import "DPI-C" context function int remove_late();
  import "DPI-C" context function int value_changes();
  import "DPI-C" context function int delay_calls();
  import "DPI-C" context function int removed_calls();
  import "DPI-C" context function int differences_so_far();

  int x = 0;

  task work();
    #1;
  endtask

  initial begin
    if (inspect() !== 0) $fatal(1, "reading the callbacks back failed");
    #1 x = 1;
    #5 if (remove_late() !== 0) $fatal(1, "removing a late callback failed");
    #5;
  end

  final begin
    if (value_changes() !== 1)
      $fatal(1, "a value-change callback was called %0d times",
             value_changes());
    if (delay_calls() !== 1)
      $fatal(1, "a callback whose handle was released was called %0d times",
             delay_calls());
    if (removed_calls() !== 0) $fatal(1, "a removed callback was called");
    if ($time !== 11) $fatal(1, "the run ended at %0t, expected 11", $time);
    if (differences_so_far() !== 0)
      $fatal(1, "the callbacks differ in %0d answers", differences_so_far());
    $display("All checks passed");
  end
endmodule

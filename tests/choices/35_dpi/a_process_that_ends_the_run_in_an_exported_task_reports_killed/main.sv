// @reports-none: error finish
//
// A process whose imported task calls an exported task that calls `$finish`
// (LRM 20.2) ended the run, and a final procedure can still read a handle to
// it (LRM 9.2.3, 9.7). What status() answers then, and what suspend() does, is
// the tool's. Lyra does not hand such a task back to its foreign caller, which
// leaves the process's frame waiting on the foreign stack until the run's end
// releases it; nothing of the design runs in it again, so it reports KILLED
// and suspending it changes nothing, as for a process that called `$finish`
// itself.
module Top;
  import "DPI-C" context task drive();

  export "DPI-C" task step;

  process ender;
  int after_the_call = 7;

  task step();
    $finish(0);
  endtask

  initial begin
    ender = process::self();
    drive();
    after_the_call = 1;
  end

  final begin
    if (ender == null) $fatal(1, "the process never ran");
    if (ender.status() != process::KILLED)
      $fatal(1, "the process that ended the run reports state %0d, not KILLED",
             ender.status());
    ender.suspend();
    if (ender.status() != process::KILLED)
      $fatal(1, "suspending it moved it to state %0d", ender.status());
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    $display("All checks passed");
  end
endmodule

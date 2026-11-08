// @reports-none: error finish
//
// `$finish` ends the simulation (LRM 20.2) and a final procedure can still
// read a handle to the process that called it (LRM 9.2.3, 9.7). What status()
// answers then is the tool's: the process neither ran out its body nor was
// killed by anything the standard names. Lyra stops the calling process at the
// call, so it reports KILLED, as a process that left its body without reaching
// its end does.
module Top;
  process ender;
  int after_the_call = 7;

  initial begin
    ender = process::self();
    $finish(0);
    after_the_call = 1;
  end

  final begin
    if (ender == null) $fatal(1, "the process never ran");
    if (ender.status() != process::KILLED)
      $fatal(1, "the process that ended the run reports state %0d, not KILLED",
             ender.status());
    if (after_the_call !== 7)
      $fatal(1, "the statement after $finish ran");
    $display("All checks passed");
  end
endmodule

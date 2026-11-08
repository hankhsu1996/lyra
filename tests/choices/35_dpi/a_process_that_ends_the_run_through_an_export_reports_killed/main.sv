// @reports-none: error finish
//
// A process whose foreign call reaches an exported function that calls
// `$finish` (LRM 20.2) ended the run as surely as one that called it itself,
// and a final procedure can still read a handle to it (LRM 9.2.3, 9.7). What
// status() answers then is the tool's. Lyra stops the process where the import
// call returns, so it reports KILLED, as a process that called `$finish`
// directly does.
module Top;
  import "DPI-C" context function void call_in();

  export "DPI-C" function ends_the_run;

  process ender;
  int after_the_call = 7;

  function void ends_the_run();
    $finish(0);
  endfunction

  initial begin
    ender = process::self();
    call_in();
    after_the_call = 1;
  end

  final begin
    if (ender == null) $fatal(1, "the process never ran");
    if (ender.status() != process::KILLED)
      $fatal(1, "the process that ended the run reports state %0d, not KILLED",
             ender.status());
    if (after_the_call !== 7)
      $fatal(1, "the statement after the import call ran");
    $display("All checks passed");
  end
endmodule

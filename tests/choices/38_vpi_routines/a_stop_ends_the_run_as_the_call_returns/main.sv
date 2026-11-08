// `vpi_control(vpiStop, n)` causes `$stop` to be executed upon return of the
// application routine (LRM 38.4), and what `$stop` does beyond suspending the
// run is the tool's (LRM 20.2). Lyra has no interactive mode to suspend into
// (LRM 38.36.3), so its `$stop` ends the run as `$finish` does: the run ends
// when the context import returns, at the time it was called, a procedure
// waiting for a later time never resumes, and `final` procedures run.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function void stop_from_foreign_code(int level);
  export "DPI-C" function keep_what_vpi_control_answered;

  bit reached_later = 0;
  time stopped_at = 0;
  int answered = -1;

  function void keep_what_vpi_control_answered(int given);
    answered = given;
  endfunction

  initial begin
    #4 stop_from_foreign_code(0);
  end

  initial begin
    #10 reached_later = 1;
  end

  final begin
    stopped_at = $time;
    if (answered !== 1)
      $fatal(1, "vpi_control(vpiStop) answered %0d, expected 1", answered);
    if (stopped_at !== 4)
      $fatal(1, "the run ended at %0t, expected 4", stopped_at);
    if (reached_later !== 0)
      $fatal(1, "a procedure waiting past the stop resumed");
    $display("All checks passed");
  end
endmodule

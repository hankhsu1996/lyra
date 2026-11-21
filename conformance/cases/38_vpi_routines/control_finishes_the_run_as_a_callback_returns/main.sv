// A callback routine is an application routine, so `vpi_control(vpiFinish, n)`
// called from one causes `$finish` upon its return (LRM 38.4): the run ends at
// the time the callback was called, a procedure waiting for a later time never
// resumes, and `final` procedures run as for any `$finish` (LRM 9.2.3).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place_finish();
  import "DPI-C" context function int answered();

  bit reached_later = 0;

  initial if (place_finish() !== 1) $fatal(1, "the callback was refused");

  initial #10 reached_later = 1;

  final begin
    if (answered() !== 1)
      $fatal(1, "vpi_control(vpiFinish) answered %0d, expected 1", answered());
    if ($time !== 5) $fatal(1, "the run ended at %0t, expected 5", $time);
    if (reached_later !== 0)
      $fatal(1, "a procedure waiting past the finish resumed");
    $display("All checks passed");
  end
endmodule

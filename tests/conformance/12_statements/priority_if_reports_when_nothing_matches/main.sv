// @reports: violation
//
// With no else to cover the remaining values, a priority-if asserts that some
// condition holds, so an execution where none does is a violation and is
// reported. No statement runs, which is what separates the report from the
// selection (LRM 12.4.2). The report is issued in the Observed region
// (LRM 12.4.2.1), and a tool may end the run there, so the check is made where
// the statement ran, before it.
module Top;
  int taken;

  initial begin
    int value;
    value = 99;

    taken = 7;
    priority if (value == 1) taken = 1;
    else if (value == 2) taken = 2;

    if (taken !== 7) $fatal(1, "taken was %0d, expected 7", taken);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule

// @reports: violation
//
// A unique0-if is violated when more than one condition is found true. An
// explicit else covers the values no condition claimed, so it answers for
// whether some condition holds and never for whether two do: overlap is
// reported with an else present exactly as without one. The statement that
// runs is still the first true condition's and only that one's (LRM 12.4.2).
// The report is issued in the Observed region (LRM 12.4.2.1), and a tool may
// end the run there, so the check is made where the statement ran, before it.
module Top;
  int taken;

  initial begin
    bit [2:0] value;
    value = 3'd3;

    taken = 0;
    unique0 if (value[0] == 1) taken = 1;
    else if (value[1] == 1) taken = 2;
    else taken = 7;

    if (taken !== 1) $fatal(1, "taken was %0d, expected 1", taken);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule

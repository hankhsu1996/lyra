// @reports: violation
//
// A unique0-case is violated when more than one case_item matches. A default
// covers the values no item claimed, so it answers for whether some item
// matches and never for whether two do: overlap is reported with a default
// present exactly as without one. The statement that runs is still the first
// matching item's and only that one's (LRM 12.5.3). The report is issued in
// the Observed region (LRM 12.5.3.1), and a tool may end the run there, so the
// check is made where the statement ran, before it.
module Top;
  int taken;

  initial begin
    bit [2:0] value;
    value = 3'd3;

    taken = 0;
    unique0 casez (value)
      3'b0?1: taken = 1;
      3'b01?: taken = 2;
      default: taken = 7;
    endcase

    if (taken !== 1) $fatal(1, "taken was %0d, expected 1", taken);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule

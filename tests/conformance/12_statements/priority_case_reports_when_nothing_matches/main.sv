// @reports: violation
//
// With no default to select, a priority-case asserts that some case_item
// matches, so an execution where none does is a violation and is reported. No
// item's statement runs (LRM 12.5.3). The report is issued in the Observed
// region (LRM 12.5.3.1), and a tool may end the run there, so the check is
// made where the statement ran, before it.
module Top;
  int taken;

  initial begin
    bit [2:0] value;
    value = 3'd5;

    taken = 7;
    priority case (value)
      3'd0, 3'd1: taken = 1;
      3'd2: taken = 2;
    endcase

    if (taken !== 7) $fatal(1, "taken was %0d, expected 7", taken);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule

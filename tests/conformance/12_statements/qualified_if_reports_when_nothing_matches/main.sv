// @reports: violation count=2
//
// With no else to cover the remaining values, a unique-if and a priority-if
// each assert that some condition holds, so an execution where none does is a
// violation and is reported. No statement runs either way, which is what
// separates the report from the selection (LRM 12.4.2). The report is issued
// in the Observed region (LRM 12.4.2.1), and a tool may end the run there, so
// the checks are made where the statements ran, before it.
module Top;
  int unique_taken;
  int priority_taken;

  initial begin
    int value;
    value = 99;

    unique_taken = 7;
    unique if (value == 1) unique_taken = 1;
    else if (value == 2) unique_taken = 2;

    priority_taken = 7;
    priority if (value == 1) priority_taken = 1;
    else if (value == 2) priority_taken = 2;

    if (unique_taken !== 7)
      $fatal(1, "unique_taken was %0d, expected 7", unique_taken);
    if (priority_taken !== 7)
      $fatal(1, "priority_taken was %0d, expected 7", priority_taken);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule

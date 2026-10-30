// The always_latch construct is identical to always_comb apart from the checks
// a tool is advised to make on the body, so it is triggered once at time zero
// and again on any change to a variable in its implicit sensitivity list (LRM
// 9.2.2.3, 9.2.2.2). A body that assigns only under a condition therefore
// follows its input while the condition holds, and leaves the target at the
// value it last took while the condition is false -- including when the input
// itself changes, which wakes the procedure but assigns nothing.
module Top;
  int d;
  int q;
  bit en;

  int at_time_zero;
  int after_input_change;

  always_latch begin
    if (en) q = d;
  end

  initial begin
    en = 1;
    d = 5;
    #1;
    at_time_zero = q;
    d = 7;
    #1;
    after_input_change = q;
    en = 0;
    #1;
    d = 99;
    #1;
  end

  final begin
    if (at_time_zero !== 5)
      $fatal(1, "at_time_zero was %0d, expected 5", at_time_zero);
    if (after_input_change !== 7)
      $fatal(1, "after_input_change was %0d, expected 7", after_input_change);
    if (q !== 7) $fatal(1, "q was %0d, expected 7", q);
    $display("All checks passed");
  end
endmodule

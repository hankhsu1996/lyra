// An if statement executes its first branch when the condition has a nonzero
// known value and its else branch when the condition is zero. The numeric
// value of the condition is what is tested, so a real condition takes the
// first branch for any nonzero value, negative ones included, and the else
// branch only for 0.0 (LRM 12.4).
module Top;
  real positive = 1.5;
  real zero = 0.0;
  real negative = -0.5;
  int from_positive;
  int from_zero;
  int from_negative;

  initial begin
    // Each branch writes a value of its own, so which branch ran is told
    // apart from neither of them having run.
    if (positive) from_positive = 1;
    else from_positive = 2;

    if (zero) from_zero = 1;
    else from_zero = 2;

    if (negative) from_negative = 1;
    else from_negative = 2;
  end

  final begin
    if (from_positive !== 1)
      $fatal(1, "from_positive was %0d, expected 1", from_positive);
    if (from_zero !== 2)
      $fatal(1, "from_zero was %0d, expected 2", from_zero);
    if (from_negative !== 1)
      $fatal(1, "from_negative was %0d, expected 1", from_negative);
    $display("All checks passed");
  end
endmodule

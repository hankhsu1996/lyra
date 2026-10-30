// The relational operators < <= > >= and the equality operators == and !=
// accept real operands and yield a single-bit value, 1'b1 when the relation
// holds and 1'b0 when it does not. If either operand is real, the other is
// converted to an equivalent real value and the comparison is carried out
// between real values (LRM 11.3.1, 11.4.4, 11.4.5, Table 11-1).
module Top;
  real low = 2.5;
  real high = 3.5;
  real low_copy = 2.5;
  real whole = 2.0;
  int whole_int = 2;
  logic eq_same;
  logic eq_different;
  logic ne_different;
  logic ne_same;
  logic greater;
  logic greater_equal;
  logic less_true;
  logic less_false;
  logic less_equal;
  logic eq_when_greater;
  logic ne_when_greater;
  logic less_when_same;
  logic less_equal_when_less;
  logic less_equal_when_greater;
  logic greater_when_same;
  logic greater_when_less;
  logic greater_equal_when_less;
  logic greater_equal_when_greater;
  logic mixed_greater;
  logic mixed_equal;
  int relational_branch;

  initial begin
    eq_same = (low == low_copy);
    eq_different = (low == high);
    ne_different = (low != high);
    ne_same = (low != low_copy);
    greater = (high > low);
    greater_equal = (low >= low_copy);
    less_true = (low < high);
    less_false = (high < low);
    less_equal = (low <= low_copy);

    // Each operator is read with a smaller, an equal and a larger left
    // operand, so none of them can pass by answering the same way whatever it
    // is given.
    eq_when_greater = (high == low);
    ne_when_greater = (high != low);
    less_when_same = (low < low_copy);
    less_equal_when_less = (low <= high);
    less_equal_when_greater = (high <= low);
    greater_when_same = (low > low_copy);
    greater_when_less = (low > high);
    greater_equal_when_less = (low >= high);
    greater_equal_when_greater = (high >= low);

    mixed_greater = (low > whole_int);
    mixed_equal = (whole == whole_int);
    if (low > whole) relational_branch = 1;
    else relational_branch = 0;
  end

  final begin
    if (eq_same !== 1'b1)
      $fatal(1, "eq_same was %b, expected 1", eq_same);
    if (eq_different !== 1'b0)
      $fatal(1, "eq_different was %b, expected 0", eq_different);
    if (ne_different !== 1'b1)
      $fatal(1, "ne_different was %b, expected 1", ne_different);
    if (ne_same !== 1'b0)
      $fatal(1, "ne_same was %b, expected 0", ne_same);
    if (greater !== 1'b1)
      $fatal(1, "greater was %b, expected 1", greater);
    if (greater_equal !== 1'b1)
      $fatal(1, "greater_equal was %b, expected 1", greater_equal);
    if (less_true !== 1'b1)
      $fatal(1, "less_true was %b, expected 1", less_true);
    if (less_false !== 1'b0)
      $fatal(1, "less_false was %b, expected 0", less_false);
    if (less_equal !== 1'b1)
      $fatal(1, "less_equal was %b, expected 1", less_equal);
    if (eq_when_greater !== 1'b0)
      $fatal(1, "eq_when_greater was %b, expected 0", eq_when_greater);
    if (ne_when_greater !== 1'b1)
      $fatal(1, "ne_when_greater was %b, expected 1", ne_when_greater);
    if (less_when_same !== 1'b0)
      $fatal(1, "less_when_same was %b, expected 0", less_when_same);
    if (less_equal_when_less !== 1'b1)
      $fatal(1, "less_equal_when_less was %b, expected 1",
             less_equal_when_less);
    if (less_equal_when_greater !== 1'b0)
      $fatal(1, "less_equal_when_greater was %b, expected 0",
             less_equal_when_greater);
    if (greater_when_same !== 1'b0)
      $fatal(1, "greater_when_same was %b, expected 0", greater_when_same);
    if (greater_when_less !== 1'b0)
      $fatal(1, "greater_when_less was %b, expected 0", greater_when_less);
    if (greater_equal_when_less !== 1'b0)
      $fatal(1, "greater_equal_when_less was %b, expected 0",
             greater_equal_when_less);
    if (greater_equal_when_greater !== 1'b1)
      $fatal(1, "greater_equal_when_greater was %b, expected 1",
             greater_equal_when_greater);
    if (mixed_greater !== 1'b1)
      $fatal(1, "mixed_greater was %b, expected 1", mixed_greater);
    if (mixed_equal !== 1'b1)
      $fatal(1, "mixed_equal was %b, expected 1", mixed_equal);
    if (relational_branch !== 1)
      $fatal(1, "relational_branch was %0d, expected 1", relational_branch);
    $display("All checks passed");
  end
endmodule

// The logical operators && || -> <-> and ! accept real operands and yield a
// single-bit value. A real operand is true when it is nonzero, whatever its
// sign, and false only when it is 0.0, so logical negation of a nonzero real
// is 1'b0 and of 0.0 is 1'b1, an implication fails only where a true
// antecedent meets a false consequent, and an equivalence holds exactly
// where the two operands share a truth value (LRM 11.3.1, 11.4.7,
// Table 11-1).
module Top;
  real first_positive = 1.5;
  real second_positive = 2.5;
  real zero = 0.0;
  real negative = -0.5;
  logic and_both_positive;
  logic and_with_zero;
  logic and_with_negative;
  logic or_with_zero;
  logic or_both_zero;
  logic not_of_positive;
  logic not_of_zero;
  logic not_of_negative;
  logic false_implies_false;
  logic false_implies_true;
  logic true_implies_false;
  logic true_implies_true;
  logic negative_implies_false;
  logic false_equivalent_false;
  logic false_equivalent_true;
  logic true_equivalent_false;
  logic true_equivalent_true;
  logic negative_equivalent_true;

  initial begin
    and_both_positive = first_positive && second_positive;
    and_with_zero = first_positive && zero;
    and_with_negative = negative && second_positive;
    or_with_zero = zero || second_positive;
    or_both_zero = zero || zero;
    not_of_positive = !second_positive;
    not_of_zero = !zero;
    not_of_negative = !negative;

    false_implies_false = zero -> zero;
    false_implies_true = zero -> second_positive;
    true_implies_false = first_positive -> zero;
    true_implies_true = first_positive -> second_positive;
    negative_implies_false = negative -> zero;

    false_equivalent_false = zero <-> zero;
    false_equivalent_true = zero <-> second_positive;
    true_equivalent_false = first_positive <-> zero;
    true_equivalent_true = first_positive <-> second_positive;
    negative_equivalent_true = negative <-> second_positive;
  end

  final begin
    if (and_both_positive !== 1'b1)
      $fatal(1, "and_both_positive was %b, expected 1", and_both_positive);
    if (and_with_zero !== 1'b0)
      $fatal(1, "and_with_zero was %b, expected 0", and_with_zero);
    if (and_with_negative !== 1'b1)
      $fatal(1, "and_with_negative was %b, expected 1", and_with_negative);
    if (or_with_zero !== 1'b1)
      $fatal(1, "or_with_zero was %b, expected 1", or_with_zero);
    if (or_both_zero !== 1'b0)
      $fatal(1, "or_both_zero was %b, expected 0", or_both_zero);
    if (not_of_positive !== 1'b0)
      $fatal(1, "not_of_positive was %b, expected 0", not_of_positive);
    if (not_of_zero !== 1'b1)
      $fatal(1, "not_of_zero was %b, expected 1", not_of_zero);
    if (not_of_negative !== 1'b0)
      $fatal(1, "not_of_negative was %b, expected 0", not_of_negative);

    if (false_implies_false !== 1'b1)
      $fatal(1, "false_implies_false was %b, expected 1",
             false_implies_false);
    if (false_implies_true !== 1'b1)
      $fatal(1, "false_implies_true was %b, expected 1", false_implies_true);
    if (true_implies_false !== 1'b0)
      $fatal(1, "true_implies_false was %b, expected 0", true_implies_false);
    if (true_implies_true !== 1'b1)
      $fatal(1, "true_implies_true was %b, expected 1", true_implies_true);
    if (negative_implies_false !== 1'b0)
      $fatal(1, "negative_implies_false was %b, expected 0",
             negative_implies_false);

    if (false_equivalent_false !== 1'b1)
      $fatal(1, "false_equivalent_false was %b, expected 1",
             false_equivalent_false);
    if (false_equivalent_true !== 1'b0)
      $fatal(1, "false_equivalent_true was %b, expected 0",
             false_equivalent_true);
    if (true_equivalent_false !== 1'b0)
      $fatal(1, "true_equivalent_false was %b, expected 0",
             true_equivalent_false);
    if (true_equivalent_true !== 1'b1)
      $fatal(1, "true_equivalent_true was %b, expected 1",
             true_equivalent_true);
    if (negative_equivalent_true !== 1'b1)
      $fatal(1, "negative_equivalent_true was %b, expected 1",
             negative_equivalent_true);
    $display("All checks passed");
  end
endmodule

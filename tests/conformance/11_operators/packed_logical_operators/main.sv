// The logical operators && || and ! read a whole operand as one truth value:
// a nonzero operand is true and a zero operand is false, and the result is
// 1'b1, 1'b0, or 1'bx when it is ambiguous. An operand whose known bits
// already settle the outcome keeps the result known -- a false operand of &&
// and a true operand of || -- while an operand that is neither certainly zero
// nor certainly nonzero makes the result x (LRM 11.4.7).
module Top;
  logic and_true_true;
  logic and_true_false;
  logic and_false_false;
  logic or_true_true;
  logic or_true_false;
  logic or_false_false;
  logic not_true;
  logic not_false;
  logic and_false_settles;
  logic and_true_with_unknown;
  logic and_unknown_unknown;
  logic or_true_settles;
  logic or_false_with_unknown;
  logic or_unknown_unknown;
  logic not_unknown;

  initial begin
    logic [3:0] a;
    logic [3:0] b;

    and_true_with_unknown = 1'b0;
    and_unknown_unknown = 1'b0;
    or_false_with_unknown = 1'b0;
    or_unknown_unknown = 1'b0;
    not_unknown = 1'b0;

    a = 4'b1010;
    b = 4'b0011;
    and_true_true = a && b;
    or_true_true = a || b;
    b = 4'b0000;
    and_true_false = a && b;
    or_true_false = a || b;
    a = 4'b0000;
    and_false_false = a && b;
    or_false_false = a || b;

    a = 4'b1010;
    not_true = !a;
    a = 4'b0000;
    not_false = !a;

    b = 4'bxxxx;
    and_false_settles = a && b;
    or_false_with_unknown = a || b;
    a = 4'b0001;
    and_true_with_unknown = a && b;
    or_true_settles = a || b;
    a = 4'bxxxx;
    and_unknown_unknown = a && b;
    or_unknown_unknown = a || b;
    not_unknown = !a;
  end

  final begin
    if (and_true_true !== 1'b1)
      $fatal(1, "and_true_true was %b, expected 1", and_true_true);
    if (and_true_false !== 1'b0)
      $fatal(1, "and_true_false was %b, expected 0", and_true_false);
    if (and_false_false !== 1'b0)
      $fatal(1, "and_false_false was %b, expected 0", and_false_false);
    if (or_true_true !== 1'b1)
      $fatal(1, "or_true_true was %b, expected 1", or_true_true);
    if (or_true_false !== 1'b1)
      $fatal(1, "or_true_false was %b, expected 1", or_true_false);
    if (or_false_false !== 1'b0)
      $fatal(1, "or_false_false was %b, expected 0", or_false_false);
    if (not_true !== 1'b0)
      $fatal(1, "not_true was %b, expected 0", not_true);
    if (not_false !== 1'b1)
      $fatal(1, "not_false was %b, expected 1", not_false);
    if (and_false_settles !== 1'b0)
      $fatal(1, "and_false_settles was %b, expected 0", and_false_settles);
    if (and_true_with_unknown !== 1'bx)
      $fatal(1, "and_true_with_unknown was %b, expected x",
             and_true_with_unknown);
    if (and_unknown_unknown !== 1'bx)
      $fatal(1, "and_unknown_unknown was %b, expected x",
             and_unknown_unknown);
    if (or_true_settles !== 1'b1)
      $fatal(1, "or_true_settles was %b, expected 1", or_true_settles);
    if (or_false_with_unknown !== 1'bx)
      $fatal(1, "or_false_with_unknown was %b, expected x",
             or_false_with_unknown);
    if (or_unknown_unknown !== 1'bx)
      $fatal(1, "or_unknown_unknown was %b, expected x", or_unknown_unknown);
    if (not_unknown !== 1'bx)
      $fatal(1, "not_unknown was %b, expected x", not_unknown);
    $display("All checks passed");
  end
endmodule

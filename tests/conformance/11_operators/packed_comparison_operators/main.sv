// The relational operators < <= > and >= and the equality operators == and !=
// yield 1'b1 when the relation holds and 1'b0 when it does not. Both operands
// signed makes the comparison a signed one; either operand unsigned makes it
// unsigned, so the same bits can compare either way. A relational operator
// whose operand holds an x or a z yields 1'bx whatever the other operand is,
// while == and != yield 1'bx only where the unknown bits leave the relation
// ambiguous (LRM 11.4.4, 11.4.5, Tables 11-8, 11-9).
module Top;
  logic equal;
  logic not_equal;
  logic less;
  logic less_equal;
  logic greater;
  logic greater_equal;
  logic equal_when_less;
  logic equal_when_greater;
  logic not_equal_when_same;
  logic not_equal_when_greater;
  logic less_when_equal;
  logic less_equal_when_less;
  logic less_equal_when_greater;
  logic greater_when_equal;
  logic greater_when_less;
  logic greater_equal_when_less;
  logic greater_equal_when_greater;
  logic signed_less;
  logic unsigned_less;
  logic equal_unknown;
  logic not_equal_unknown;
  logic less_unknown;
  logic less_equal_unknown;
  logic greater_unknown;
  logic greater_equal_unknown;

  logic equal_known_mismatch;
  logic not_equal_known_mismatch;

  initial begin
    logic signed [7:0] a;
    logic signed [7:0] b;
    logic [7:0] unsigned_bits;

    equal_unknown = 1'b0;
    not_equal_unknown = 1'b0;
    less_unknown = 1'b0;
    less_equal_unknown = 1'b0;
    greater_unknown = 1'b0;
    greater_equal_unknown = 1'b0;

    a = 5;
    b = 5;
    equal = (a == b);
    b = 7;
    not_equal = (a != b);
    a = -3;
    b = 4;
    less = (a < b);
    a = 4;
    b = 4;
    less_equal = (a <= b);
    a = 7;
    b = 2;
    greater = (a > b);
    a = -1;
    b = -1;
    greater_equal = (a >= b);

    // Each operator is read with a smaller, an equal and a larger left
    // operand, so none of them can pass by answering the same way whatever it
    // is given.
    a = 2;
    b = 7;
    equal_when_less = (a == b);
    less_equal_when_less = (a <= b);
    greater_when_less = (a > b);
    greater_equal_when_less = (a >= b);
    a = 5;
    b = 5;
    not_equal_when_same = (a != b);
    less_when_equal = (a < b);
    greater_when_equal = (a > b);
    a = 7;
    b = 2;
    equal_when_greater = (a == b);
    not_equal_when_greater = (a != b);
    less_equal_when_greater = (a <= b);
    greater_equal_when_greater = (a >= b);

    // The same bit pattern read as signed and as unsigned, so only the
    // signedness rule separates the two results.
    a = 8'sb11111101;
    unsigned_bits = 8'b11111101;
    signed_less = (a < 4);
    unsigned_less = (unsigned_bits < 4);

    a = 8'b000000xz;
    b = 8'b00000010;
    equal_unknown = (a == b);
    not_equal_unknown = (a != b);
    less_unknown = (a < b);
    less_equal_unknown = (a <= b);
    greater_unknown = (a > b);
    greater_equal_unknown = (a >= b);

    b = 8'b10000010;
    equal_known_mismatch = (a == b);
    not_equal_known_mismatch = (a != b);
  end

  final begin
    if (equal !== 1'b1) $fatal(1, "equal was %b, expected 1", equal);
    if (not_equal !== 1'b1)
      $fatal(1, "not_equal was %b, expected 1", not_equal);
    if (less !== 1'b1) $fatal(1, "less was %b, expected 1", less);
    if (less_equal !== 1'b1)
      $fatal(1, "less_equal was %b, expected 1", less_equal);
    if (greater !== 1'b1) $fatal(1, "greater was %b, expected 1", greater);
    if (greater_equal !== 1'b1)
      $fatal(1, "greater_equal was %b, expected 1", greater_equal);
    if (equal_when_less !== 1'b0)
      $fatal(1, "equal_when_less was %b, expected 0", equal_when_less);
    if (equal_when_greater !== 1'b0)
      $fatal(1, "equal_when_greater was %b, expected 0", equal_when_greater);
    if (not_equal_when_same !== 1'b0)
      $fatal(1, "not_equal_when_same was %b, expected 0", not_equal_when_same);
    if (not_equal_when_greater !== 1'b1)
      $fatal(1, "not_equal_when_greater was %b, expected 1",
             not_equal_when_greater);
    if (less_when_equal !== 1'b0)
      $fatal(1, "less_when_equal was %b, expected 0", less_when_equal);
    if (less_equal_when_less !== 1'b1)
      $fatal(1, "less_equal_when_less was %b, expected 1",
             less_equal_when_less);
    if (less_equal_when_greater !== 1'b0)
      $fatal(1, "less_equal_when_greater was %b, expected 0",
             less_equal_when_greater);
    if (greater_when_equal !== 1'b0)
      $fatal(1, "greater_when_equal was %b, expected 0", greater_when_equal);
    if (greater_when_less !== 1'b0)
      $fatal(1, "greater_when_less was %b, expected 0", greater_when_less);
    if (greater_equal_when_less !== 1'b0)
      $fatal(1, "greater_equal_when_less was %b, expected 0",
             greater_equal_when_less);
    if (greater_equal_when_greater !== 1'b1)
      $fatal(1, "greater_equal_when_greater was %b, expected 1",
             greater_equal_when_greater);
    if (signed_less !== 1'b1)
      $fatal(1, "signed_less was %b, expected 1", signed_less);
    if (unsigned_less !== 1'b0)
      $fatal(1, "unsigned_less was %b, expected 0", unsigned_less);
    if (equal_unknown !== 1'bx)
      $fatal(1, "equal_unknown was %b, expected x", equal_unknown);
    if (not_equal_unknown !== 1'bx)
      $fatal(1, "not_equal_unknown was %b, expected x", not_equal_unknown);
    if (less_unknown !== 1'bx)
      $fatal(1, "less_unknown was %b, expected x", less_unknown);
    if (less_equal_unknown !== 1'bx)
      $fatal(1, "less_equal_unknown was %b, expected x", less_equal_unknown);
    if (greater_unknown !== 1'bx)
      $fatal(1, "greater_unknown was %b, expected x", greater_unknown);
    if (greater_equal_unknown !== 1'bx)
      $fatal(1, "greater_equal_unknown was %b, expected x",
             greater_equal_unknown);

    if (equal_known_mismatch !== 1'b0)
      $fatal(1, "equal_known_mismatch was %b, expected 0",
             equal_known_mismatch);
    if (not_equal_known_mismatch !== 1'b1)
      $fatal(1, "not_equal_known_mismatch was %b, expected 1",
             not_equal_known_mismatch);
    $display("All checks passed");
  end
endmodule

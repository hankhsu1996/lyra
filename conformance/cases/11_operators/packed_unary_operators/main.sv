// Unary plus leaves its operand's value unchanged and unary minus negates it
// within the operand's width, so negating an unsigned vector wraps around
// modulo two to the power of that width. Unary ~ negates each bit and unary !
// reduces the whole operand to one truth bit. An x or z bit makes the whole
// arithmetic result x and makes the truth value ambiguous, while ~ turns it
// into x only in the positions it occupies (LRM 11.4.3, 11.4.7, 11.4.8,
// Tables 11-6, 11-15).
module Top;
  logic [3:0] unary_plus;
  logic [3:0] unary_minus;
  logic [3:0] bitwise_not;
  logic not_nonzero;
  logic not_zero;
  logic [3:0] unary_minus_unknown;
  logic [3:0] bitwise_not_unknown;
  logic not_unknown;

  initial begin
    logic [3:0] a;

    unary_minus_unknown = 4'b0000;
    not_unknown = 1'b0;

    a = 4'b1010;
    unary_plus = +a;
    unary_minus = -a;
    bitwise_not = ~a;
    not_nonzero = !a;

    a = 4'b0000;
    not_zero = !a;

    // Zeros above the unknown bits, so the value is neither certainly zero
    // nor certainly nonzero.
    a = 4'b00xx;
    unary_minus_unknown = -a;
    bitwise_not_unknown = ~a;
    not_unknown = !a;
  end

  final begin
    if (unary_plus !== 4'b1010)
      $fatal(1, "unary_plus was %b, expected 1010", unary_plus);
    if (unary_minus !== 4'b0110)
      $fatal(1, "unary_minus was %b, expected 0110", unary_minus);
    if (bitwise_not !== 4'b0101)
      $fatal(1, "bitwise_not was %b, expected 0101", bitwise_not);
    if (not_nonzero !== 1'b0)
      $fatal(1, "not_nonzero was %b, expected 0", not_nonzero);
    if (not_zero !== 1'b1)
      $fatal(1, "not_zero was %b, expected 1", not_zero);
    if (unary_minus_unknown !== 4'bxxxx)
      $fatal(1, "unary_minus_unknown was %b, expected xxxx",
             unary_minus_unknown);
    if (bitwise_not_unknown !== 4'b11xx)
      $fatal(1, "bitwise_not_unknown was %b, expected 11xx",
             bitwise_not_unknown);
    if (not_unknown !== 1'bx)
      $fatal(1, "not_unknown was %b, expected x", not_unknown);
    $display("All checks passed");
  end
endmodule

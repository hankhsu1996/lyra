// The left shift operators << and <<< move their left operand towards the
// most significant end and fill the vacated positions with zeros; the logical
// right shift >> fills them with zeros as well. Only the arithmetic right
// shift >>> fills them with the sign bit of its left operand, and only when
// the result type is signed. The right operand is treated as unsigned and
// does not affect the result's signedness, and an x or z anywhere in it makes
// the whole result unknown; bits shifted out are lost (LRM 11.4.10).
module Top;
  logic [7:0] left_logical;
  logic [7:0] right_logical;
  logic signed [7:0] arithmetic_right_positive;
  logic signed [7:0] arithmetic_right_negative;
  logic [7:0] logical_right_negative;
  logic [7:0] left_arithmetic_negative;
  logic [3:0] left_past_width;
  logic [3:0] right_past_width;
  logic [7:0] unknown_amount;
  logic [7:0] left_keeps_unknown;
  logic [7:0] right_keeps_unknown;
  logic signed [7:0] arithmetic_right_keeps_unknown;
  logic signed [7:0] arithmetic_right_unknown_sign;

  initial begin
    logic [7:0] a;
    logic signed [7:0] s;
    logic [3:0] narrow;
    logic [7:0] v;
    logic [3:0] amount;

    unknown_amount = 8'h00;

    a = 8'b10110100;
    left_logical = a << 2;
    right_logical = a >> 2;

    s = 8'sb00110100;
    arithmetic_right_positive = s >>> 2;
    // A negative left operand, so filling with the sign bit differs from
    // filling with zeros.
    s = 8'sb11001100;
    arithmetic_right_negative = s >>> 2;
    logical_right_negative = s >> 2;
    left_arithmetic_negative = s <<< 2;

    narrow = 4'b1010;
    left_past_width = narrow << 8;
    right_past_width = narrow >> 8;

    v = 8'b00000001;
    amount = 4'bxxxx;
    unknown_amount = v << amount;

    v = 8'b1011xx00;
    left_keeps_unknown = v << 1;
    right_keeps_unknown = v >> 1;

    s = 8'sb1011xx00;
    arithmetic_right_keeps_unknown = s >>> 1;
    s = 8'sbx0001110;
    arithmetic_right_unknown_sign = s >>> 2;
  end

  final begin
    if (left_logical !== 8'b11010000)
      $fatal(1, "left_logical was %b, expected 11010000", left_logical);
    if (right_logical !== 8'b00101101)
      $fatal(1, "right_logical was %b, expected 00101101", right_logical);
    if (arithmetic_right_positive !== 8'b00001101)
      $fatal(1, "arithmetic_right_positive was %b, expected 00001101",
             arithmetic_right_positive);
    if (arithmetic_right_negative !== 8'b11110011)
      $fatal(1, "arithmetic_right_negative was %b, expected 11110011",
             arithmetic_right_negative);
    if (logical_right_negative !== 8'b00110011)
      $fatal(1, "logical_right_negative was %b, expected 00110011",
             logical_right_negative);
    if (left_arithmetic_negative !== 8'b00110000)
      $fatal(1, "left_arithmetic_negative was %b, expected 00110000",
             left_arithmetic_negative);
    if (left_past_width !== 4'b0000)
      $fatal(1, "left_past_width was %b, expected 0000", left_past_width);
    if (right_past_width !== 4'b0000)
      $fatal(1, "right_past_width was %b, expected 0000", right_past_width);
    if (unknown_amount !== 8'bxxxxxxxx)
      $fatal(1, "unknown_amount was %b, expected xxxxxxxx", unknown_amount);
    if (left_keeps_unknown !== 8'b011xx000)
      $fatal(1, "left_keeps_unknown was %b, expected 011xx000",
             left_keeps_unknown);
    if (right_keeps_unknown !== 8'b01011xx0)
      $fatal(1, "right_keeps_unknown was %b, expected 01011xx0",
             right_keeps_unknown);
    if (arithmetic_right_keeps_unknown !== 8'b11011xx0)
      $fatal(1, "arithmetic_right_keeps_unknown was %b, expected 11011xx0",
             arithmetic_right_keeps_unknown);
    if (arithmetic_right_unknown_sign !== 8'bxxx00011)
      $fatal(1, "arithmetic_right_unknown_sign was %b, expected xxx00011",
             arithmetic_right_unknown_sign);
    $display("All checks passed");
  end
endmodule

// The power operator ** raises its first operand to the power of its second,
// and the second operand is self-determined. For integral operands the result
// is 1 whenever the second operand is zero. When the second operand is
// negative the result is 1 for a first operand of 1, is 1 or -1 for a first
// operand of -1 according to whether the exponent is even or odd, is x
// throughout for a first operand of zero, and is 0 for a first operand outside
// the range -1 to 1. Otherwise an x or z bit in either operand makes the whole
// result x (LRM 11.4.3, Tables 11-4, 11-5).
module Top;
  logic signed [15:0] positive_exponent;
  logic signed [15:0] small_positive_exponent;
  logic signed [15:0] zero_exponent;
  logic signed [15:0] zero_base_zero_exponent;
  logic signed [15:0] one_base_negative_exponent;
  logic signed [15:0] minus_one_base_even_negative_exponent;
  logic signed [15:0] minus_one_base_odd_negative_exponent;
  logic signed [15:0] large_base_negative_exponent;
  logic signed [15:0] small_base_negative_exponent;
  logic signed [15:0] unknown_base;
  logic signed [15:0] unknown_exponent;

  logic signed [15:0] zero_base_negative_exponent;

  initial begin
    logic signed [15:0] a;
    logic signed [15:0] b;

    unknown_base = 16'd0;
    unknown_exponent = 16'd0;
    zero_base_negative_exponent = 16'd0;

    a = 3;
    b = 5;
    positive_exponent = a ** b;
    a = 2;
    b = 3;
    small_positive_exponent = a ** b;
    a = 7;
    b = 0;
    zero_exponent = a ** b;
    a = 0;
    zero_base_zero_exponent = a ** b;

    a = 1;
    b = -3;
    one_base_negative_exponent = a ** b;
    a = -1;
    b = -4;
    minus_one_base_even_negative_exponent = a ** b;
    b = -3;
    minus_one_base_odd_negative_exponent = a ** b;

    a = 0;
    b = -1;
    zero_base_negative_exponent = a ** b;

    a = 2;
    b = -3;
    large_base_negative_exponent = a ** b;
    a = -2;
    small_base_negative_exponent = a ** b;

    a = 16'bxxxxxxxxxxxxxxxx;
    b = 2;
    unknown_base = a ** b;
    a = 2;
    b = 16'bxxxxxxxxxxxxxxxx;
    unknown_exponent = a ** b;
  end

  final begin
    if (positive_exponent !== 243)
      $fatal(1, "positive_exponent was %0d, expected 243", positive_exponent);
    if (small_positive_exponent !== 8)
      $fatal(1, "small_positive_exponent was %0d, expected 8",
             small_positive_exponent);
    if (zero_exponent !== 1)
      $fatal(1, "zero_exponent was %0d, expected 1", zero_exponent);
    if (zero_base_zero_exponent !== 1)
      $fatal(1, "zero_base_zero_exponent was %0d, expected 1",
             zero_base_zero_exponent);
    if (one_base_negative_exponent !== 1)
      $fatal(1, "one_base_negative_exponent was %0d, expected 1",
             one_base_negative_exponent);
    if (minus_one_base_even_negative_exponent !== 1)
      $fatal(1, "minus_one_base_even_negative_exponent was %0d, expected 1",
             minus_one_base_even_negative_exponent);
    if (minus_one_base_odd_negative_exponent !== -1)
      $fatal(1, "minus_one_base_odd_negative_exponent was %0d, expected -1",
             minus_one_base_odd_negative_exponent);
    if (large_base_negative_exponent !== 0)
      $fatal(1, "large_base_negative_exponent was %0d, expected 0",
             large_base_negative_exponent);
    if (small_base_negative_exponent !== 0)
      $fatal(1, "small_base_negative_exponent was %0d, expected 0",
             small_base_negative_exponent);
    if (unknown_base !== 16'bxxxxxxxxxxxxxxxx)
      $fatal(1, "unknown_base was %b, expected all x", unknown_base);
    if (unknown_exponent !== 16'bxxxxxxxxxxxxxxxx)
      $fatal(1, "unknown_exponent was %b, expected all x", unknown_exponent);

    if (zero_base_negative_exponent !== 16'bxxxxxxxxxxxxxxxx)
      $fatal(1, "zero_base_negative_exponent was %b, expected all x",
             zero_base_negative_exponent);
    $display("All checks passed");
  end
endmodule

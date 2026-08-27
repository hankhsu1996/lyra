// The arithmetic operators + - * / and % combine integral operands, reading
// an operand declared signed as a two's-complement signed value. Integer
// division truncates its fractional part towards zero, and a modulus takes
// the sign of its first operand. If any bit of either operand is x or z, or
// if the second operand of / or % is zero, the entire result is x
// (LRM 11.4.3, 11.4.3.1, Tables 11-3, 11-5, 11-7).
module Top;
  logic signed [7:0] sum;
  logic signed [7:0] difference;
  logic signed [7:0] negative_difference;
  logic signed [7:0] product;
  logic signed [7:0] negative_product;
  logic signed [7:0] quotient;
  logic signed [7:0] negative_quotient;
  logic signed [7:0] remainder;
  logic signed [7:0] negative_dividend_remainder;
  logic signed [7:0] negative_divisor_remainder;
  logic signed [7:0] sum_unknown;
  logic signed [7:0] difference_unknown;
  logic signed [7:0] product_unknown;
  logic signed [7:0] quotient_unknown;
  logic signed [7:0] remainder_unknown;
  logic signed [7:0] divide_by_zero;
  logic signed [7:0] modulo_by_zero;

  initial begin
    logic signed [7:0] a;
    logic signed [7:0] b;

    sum_unknown = 8'h00;
    difference_unknown = 8'h00;
    product_unknown = 8'h00;
    quotient_unknown = 8'h00;
    remainder_unknown = 8'h00;
    divide_by_zero = 8'h00;
    modulo_by_zero = 8'h00;

    a = 30;
    b = 12;
    sum = a + b;
    difference = a - b;
    negative_difference = b - a;

    a = 5;
    b = 7;
    product = a * b;
    a = -5;
    negative_product = a * b;

    // A dividend that does not divide exactly, so truncation towards zero is
    // told apart from rounding away from it.
    a = 103;
    b = 4;
    quotient = a / b;
    a = -103;
    negative_quotient = a / b;

    a = 17;
    b = 5;
    remainder = a % b;
    a = -10;
    b = 3;
    negative_dividend_remainder = a % b;
    a = 11;
    b = -3;
    negative_divisor_remainder = a % b;

    a = 8'b000000xz;
    b = 8'b00000010;
    sum_unknown = a + b;
    difference_unknown = a - b;
    product_unknown = a * b;
    quotient_unknown = a / b;
    remainder_unknown = a % b;

    a = 17;
    b = 0;
    divide_by_zero = a / b;
    modulo_by_zero = a % b;
  end

  final begin
    if (sum !== 42) $fatal(1, "sum was %0d, expected 42", sum);
    if (difference !== 18)
      $fatal(1, "difference was %0d, expected 18", difference);
    if (negative_difference !== -18)
      $fatal(1, "negative_difference was %0d, expected -18",
             negative_difference);
    if (product !== 35) $fatal(1, "product was %0d, expected 35", product);
    if (negative_product !== -35)
      $fatal(1, "negative_product was %0d, expected -35", negative_product);
    if (quotient !== 25)
      $fatal(1, "quotient was %0d, expected 25", quotient);
    if (negative_quotient !== -25)
      $fatal(1, "negative_quotient was %0d, expected -25", negative_quotient);
    if (remainder !== 2)
      $fatal(1, "remainder was %0d, expected 2", remainder);
    if (negative_dividend_remainder !== -1)
      $fatal(1, "negative_dividend_remainder was %0d, expected -1",
             negative_dividend_remainder);
    if (negative_divisor_remainder !== 2)
      $fatal(1, "negative_divisor_remainder was %0d, expected 2",
             negative_divisor_remainder);
    if (sum_unknown !== 8'bxxxxxxxx)
      $fatal(1, "sum_unknown was %b, expected xxxxxxxx", sum_unknown);
    if (difference_unknown !== 8'bxxxxxxxx)
      $fatal(1, "difference_unknown was %b, expected xxxxxxxx",
             difference_unknown);
    if (product_unknown !== 8'bxxxxxxxx)
      $fatal(1, "product_unknown was %b, expected xxxxxxxx", product_unknown);
    if (quotient_unknown !== 8'bxxxxxxxx)
      $fatal(1, "quotient_unknown was %b, expected xxxxxxxx",
             quotient_unknown);
    if (remainder_unknown !== 8'bxxxxxxxx)
      $fatal(1, "remainder_unknown was %b, expected xxxxxxxx",
             remainder_unknown);
    if (divide_by_zero !== 8'bxxxxxxxx)
      $fatal(1, "divide_by_zero was %b, expected xxxxxxxx", divide_by_zero);
    if (modulo_by_zero !== 8'bxxxxxxxx)
      $fatal(1, "modulo_by_zero was %b, expected xxxxxxxx", modulo_by_zero);
    $display("All checks passed");
  end
endmodule

// The arithmetic operators + - * / and % combine integral operands, reading
// an operand declared signed as a two's-complement signed value. Integer
// division truncates its fractional part towards zero, and a modulus takes
// the sign of its first operand. If any bit of either operand is x or z, or
// if the second operand of / or % is zero, the entire result is x. A result
// is as wide as its operands and a carry out of the most significant bit is
// lost, at any width
// (LRM 11.4.3, 11.4.3.1, 11.6.1, Tables 11-3, 11-5, 11-7).
module Top;
  logic [0:0] sum_wraps_1;
  logic [6:0] sum_wraps_7;
  logic [32:0] sum_carries_into_top_33;
  logic [32:0] sum_wraps_33;
  logic [62:0] sum_wraps_63;
  logic [63:0] sum_wraps_64;
  logic [63:0] difference_borrows_64;
  logic [32:0] product_wraps_33;
  logic [62:0] product_63;
  logic [63:0] product_wraps_64;
  logic [63:0] quotient_64;
  logic [63:0] remainder_64;
  logic signed [32:0] negative_quotient_33;
  logic signed [32:0] negative_remainder_33;
  logic signed [6:0] negative_quotient_7;
  logic [32:0] sum_unknown_33;
  logic [63:0] divide_by_zero_64;

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

  initial begin
    logic [0:0] a1;
    logic [6:0] a7;
    logic signed [6:0] s7;
    logic [32:0] a33;
    logic [32:0] b33;
    logic signed [32:0] s33;
    logic [62:0] a63;
    logic [63:0] a64;
    logic [63:0] b64;

    sum_unknown_33 = '0;
    divide_by_zero_64 = '0;

    a1 = 1'b1;
    sum_wraps_1 = a1 + a1;
    a7 = 7'h7f;
    sum_wraps_7 = a7 + 7'd1;
    a33 = 33'h0_ffff_ffff;
    sum_carries_into_top_33 = a33 + 33'd1;
    a33 = 33'h1_ffff_ffff;
    sum_wraps_33 = a33 + 33'd1;
    a63 = 63'h7fff_ffff_ffff_ffff;
    sum_wraps_63 = a63 + 63'd1;
    product_63 = a63 * 63'd2;
    a64 = 64'hffff_ffff_ffff_ffff;
    sum_wraps_64 = a64 + 64'd1;
    product_wraps_64 = a64 * a64;
    quotient_64 = a64 / 64'd16;
    remainder_64 = a64 % 64'd16;
    b64 = 64'd0;
    difference_borrows_64 = b64 - 64'd1;
    divide_by_zero_64 = a64 / b64;

    a33 = 33'h1_0000_0001;
    b33 = 33'd3;
    product_wraps_33 = a33 * b33;
    s33 = -33'sd10;
    negative_quotient_33 = s33 / 33'sd3;
    negative_remainder_33 = s33 % 33'sd3;
    s7 = -7'sd64;
    negative_quotient_7 = s7 / 7'sd2;

    a33 = 33'h0_0000_0005;
    a33[32] = 1'bx;
    sum_unknown_33 = a33 + 33'd1;
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

    if (sum_wraps_1 !== 1'b0)
      $fatal(1, "sum_wraps_1 was %b, expected 0", sum_wraps_1);
    if (sum_wraps_7 !== 7'h00)
      $fatal(1, "sum_wraps_7 was %h, expected 00", sum_wraps_7);
    if (sum_carries_into_top_33 !== 33'h1_0000_0000)
      $fatal(1, "sum_carries_into_top_33 was %h, expected 100000000",
             sum_carries_into_top_33);
    if (sum_wraps_33 !== 33'h0_0000_0000)
      $fatal(1, "sum_wraps_33 was %h, expected 000000000", sum_wraps_33);
    if (sum_wraps_63 !== 63'h0)
      $fatal(1, "sum_wraps_63 was %h, expected 0", sum_wraps_63);
    if (sum_wraps_64 !== 64'h0)
      $fatal(1, "sum_wraps_64 was %h, expected 0", sum_wraps_64);
    if (difference_borrows_64 !== 64'hffff_ffff_ffff_ffff)
      $fatal(1, "difference_borrows_64 was %h, expected ffffffffffffffff",
             difference_borrows_64);
    if (product_wraps_33 !== 33'h1_0000_0003)
      $fatal(1, "product_wraps_33 was %h, expected 100000003",
             product_wraps_33);
    if (product_63 !== 63'h7fff_ffff_ffff_fffe)
      $fatal(1, "product_63 was %h, expected 7ffffffffffffffe", product_63);
    if (product_wraps_64 !== 64'h1)
      $fatal(1, "product_wraps_64 was %h, expected 1", product_wraps_64);
    if (quotient_64 !== 64'h0fff_ffff_ffff_ffff)
      $fatal(1, "quotient_64 was %h, expected 0fffffffffffffff", quotient_64);
    if (remainder_64 !== 64'd15)
      $fatal(1, "remainder_64 was %0d, expected 15", remainder_64);
    if (negative_quotient_33 !== -33'sd3)
      $fatal(1, "negative_quotient_33 was %0d, expected -3",
             negative_quotient_33);
    if (negative_remainder_33 !== -33'sd1)
      $fatal(1, "negative_remainder_33 was %0d, expected -1",
             negative_remainder_33);
    if (negative_quotient_7 !== -7'sd32)
      $fatal(1, "negative_quotient_7 was %0d, expected -32",
             negative_quotient_7);
    if (sum_unknown_33 !== 33'bx)
      $fatal(1, "sum_unknown_33 was %b, expected every bit x", sum_unknown_33);
    if (divide_by_zero_64 !== 64'bx)
      $fatal(1, "divide_by_zero_64 was %b, expected every bit x",
             divide_by_zero_64);
    $display("All checks passed");
  end
endmodule

// The arithmetic operators are defined on integral operands of any width.
// The result is computed in the width of the expression, so a value that
// does not fit wraps within it. Integer division truncates the fractional
// part toward zero and a modulus takes the sign of its first operand; if the
// second operand of either is zero the whole result is x, which a 2-state
// result carries as 0. The power operator follows the integral power rules
// (LRM 11.4.3, Table 11-4, Table 11-5, 11.3.4).
module Top;
  bit [64:0] sum65;
  bit [95:0] literal96;
  bit [199:0] literal200;
  bit [127:0] sum128;
  bit [127:0] diff128;
  bit [127:0] product;
  bit signed [127:0] negated;
  bit [127:0] wrapped_sum;
  bit [127:0] div_small;
  bit [127:0] mod_small;
  bit [127:0] div_cross_word;
  bit [127:0] div_max_by_two;
  bit signed [127:0] div_truncates_toward_zero;
  bit signed [127:0] mod_negative_dividend;
  bit signed [127:0] mod_negative_divisor;
  bit signed [127:0] div_both_negative;
  bit [127:0] div_by_zero_2state;
  bit [127:0] mod_by_zero_2state;
  logic [127:0] div_by_zero_4state;
  bit [127:0] pow_small;
  bit [127:0] pow_two_64;
  bit [127:0] pow_two_127;
  bit [127:0] pow_wraps;
  bit signed [127:0] pow_neg_one_even;
  bit signed [127:0] pow_neg_one_odd;
  bit [127:0] pow_exponent_zero;
  bit [127:0] pow_base_zero;
  bit [127:0] pow_negative_exponent;

  logic [127:0] pow_zero_negative_exponent;

  initial begin
    div_by_zero_2state = 128'd1;
    mod_by_zero_2state = 128'd1;
    mod_small = 128'd1;
    pow_base_zero = 128'd1;
    pow_negative_exponent = 128'd1;
    pow_wraps = 128'd1;
    div_by_zero_4state = 128'd0;

    pow_zero_negative_exponent = 128'd0;

    begin
      bit [64:0] left65;
      bit [64:0] right65;
      bit [127:0] left128;
      bit [127:0] right128;
      bit [127:0] all_ones64;
      bit signed [127:0] signed_left;
      bit signed [127:0] signed_right;
      logic [127:0] unknown_zero;
      logic [127:0] unknown_dividend;
      int exponent;

      left65 = 65'd100;
      right65 = 65'd200;
      sum65 = left65 + right65;

      literal96 = 96'd9876543210;
      literal200 = 200'd42;

      left128 = 128'd1000000000000;
      right128 = 128'd2000000000000;
      sum128 = left128 + right128;

      left128 = 128'd5000000000000;
      right128 = 128'd2000000000000;
      diff128 = left128 - right128;

      all_ones64 = 128'hFFFFFFFFFFFFFFFF;
      product = all_ones64 * all_ones64;

      left128 = 128'd100;
      negated = -left128;

      left128 = 128'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF;
      wrapped_sum = left128 + 128'd2;

      left128 = 128'd10;
      right128 = 128'd4;
      div_small = left128 / right128;
      mod_small = left128 % right128;

      left128 = 128'h2_0000_0000_0000_0000;
      right128 = 128'd2;
      div_cross_word = left128 / right128;

      left128 = 128'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF;
      div_max_by_two = left128 / 128'd2;

      signed_left = -128'sd7;
      signed_right = 128'sd3;
      div_truncates_toward_zero = signed_left / signed_right;
      mod_negative_dividend = signed_left % signed_right;

      signed_left = 128'sd7;
      signed_right = -128'sd3;
      mod_negative_divisor = signed_left % signed_right;

      signed_left = -128'sd40;
      signed_right = -128'sd5;
      div_both_negative = signed_left / signed_right;

      left128 = 128'd42;
      right128 = 128'd0;
      div_by_zero_2state = left128 / right128;
      mod_by_zero_2state = left128 % right128;

      unknown_dividend = 128'd42;
      unknown_zero = 128'd0;
      div_by_zero_4state = unknown_dividend / unknown_zero;

      left128 = 128'd3;
      exponent = 4;
      pow_small = left128 ** exponent;

      left128 = 128'd2;
      exponent = 64;
      pow_two_64 = left128 ** exponent;
      exponent = 127;
      pow_two_127 = left128 ** exponent;
      exponent = 128;
      pow_wraps = left128 ** exponent;
      exponent = -1;
      pow_negative_exponent = left128 ** exponent;

      signed_left = -128'sd1;
      exponent = 10;
      pow_neg_one_even = signed_left ** exponent;
      exponent = 11;
      pow_neg_one_odd = signed_left ** exponent;

      left128 = 128'd0;
      exponent = 0;
      pow_exponent_zero = left128 ** exponent;
      exponent = 5;
      pow_base_zero = left128 ** exponent;

      unknown_zero = 128'd0;
      exponent = -1;
      pow_zero_negative_exponent = unknown_zero ** exponent;
    end
  end

  final begin
    if (sum65 !== 65'd300)
      $fatal(1, "65-bit 100 + 200 gave %h, expected 12c", sum65);
    if (literal96 !== 96'h24CB016EA)
      $fatal(1, "96-bit 9876543210 was %h, expected 24cb016ea", literal96);
    if (literal200 !== 200'd42)
      $fatal(1, "200-bit 42 was %h, expected 2a", literal200);
    if (sum128 !== 128'h2BA7DEF3000)
      $fatal(1, "128-bit 1e12 + 2e12 gave %h, expected 2ba7def3000", sum128);
    if (diff128 !== 128'h2BA7DEF3000)
      $fatal(1, "128-bit 5e12 - 2e12 gave %h, expected 2ba7def3000", diff128);
    if (product !== 128'hFFFFFFFFFFFFFFFE0000000000000001)
      $fatal(1, "(2**64-1) squared gave %h", product);
    if (negated !== -128'sd100)
      $fatal(1, "negating 100 gave %h, expected -100", negated);
    if (wrapped_sum !== 128'd1)
      $fatal(1, "all ones + 2 wrapped to %h, expected 1", wrapped_sum);
    if (div_small !== 128'd2)
      $fatal(1, "10 / 4 gave %h, expected 2", div_small);
    if (mod_small !== 128'd2)
      $fatal(1, "10 %% 4 gave %h, expected 2", mod_small);
    if (div_cross_word !== 128'h10000000000000000)
      $fatal(1, "a division across the word boundary gave %h", div_cross_word);
    if (div_max_by_two !== 128'h7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
      $fatal(1, "all ones / 2 gave %h", div_max_by_two);
    if (div_truncates_toward_zero !== -128'sd2)
      $fatal(1, "-7 / 3 gave %0d, expected -2", div_truncates_toward_zero);
    if (mod_negative_dividend !== -128'sd1)
      $fatal(1, "-7 %% 3 gave %0d, expected -1", mod_negative_dividend);
    if (mod_negative_divisor !== 128'sd1)
      $fatal(1, "7 %% -3 gave %0d, expected 1", mod_negative_divisor);
    if (div_both_negative !== 128'sd8)
      $fatal(1, "-40 / -5 gave %0d, expected 8", div_both_negative);
    if (div_by_zero_2state !== 128'd0)
      $fatal(1, "a 2-state division by zero gave %h, expected 0",
             div_by_zero_2state);
    if (mod_by_zero_2state !== 128'd0)
      $fatal(1, "a 2-state modulus by zero gave %h, expected 0",
             mod_by_zero_2state);
    if (div_by_zero_4state !== {128{1'bx}})
      $fatal(1, "a 4-state division by zero gave %h, expected all x",
             div_by_zero_4state);
    if (pow_small !== 128'h51)
      $fatal(1, "3 ** 4 gave %h, expected 51", pow_small);
    if (pow_two_64 !== 128'h10000000000000000)
      $fatal(1, "2 ** 64 gave %h", pow_two_64);
    if (pow_two_127 !== 128'h80000000000000000000000000000000)
      $fatal(1, "2 ** 127 gave %h", pow_two_127);
    if (pow_wraps !== 128'd0)
      $fatal(1, "2 ** 128 wrapped to %h, expected 0", pow_wraps);
    if (pow_neg_one_even !== 128'sd1)
      $fatal(1, "-1 ** 10 gave %0d, expected 1", pow_neg_one_even);
    if (pow_neg_one_odd !== -128'sd1)
      $fatal(1, "-1 ** 11 gave %0d, expected -1", pow_neg_one_odd);
    if (pow_exponent_zero !== 128'd1)
      $fatal(1, "0 ** 0 gave %h, expected 1", pow_exponent_zero);
    if (pow_base_zero !== 128'd0)
      $fatal(1, "0 ** 5 gave %h, expected 0", pow_base_zero);
    if (pow_negative_exponent !== 128'd0)
      $fatal(1, "2 ** -1 gave %h, expected 0", pow_negative_exponent);

    if (pow_zero_negative_exponent !== {128{1'bx}})
      $fatal(1, "0 ** -1 gave %h, expected all x",
             pow_zero_negative_exponent);
    $display("All checks passed");
  end
endmodule

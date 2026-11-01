// A real is a C double and a shortreal a C float, each represented as IEEE
// 754 describes (LRM 6.12), and the standard says nothing of what an operation
// gives for the values that representation holds beside the numbers -- a NaN,
// an infinity and a negative zero, which a division by zero and a negation
// reach -- nor whether a product and the sum reading it may be rounded once,
// as a fused multiply-add rounds them. Lyra gives each operation the IEEE 754
// answer of its format, rounded on its own: every ordered comparison with a
// NaN is false and `!=` true, a NaN is true as a condition, an infinity lies
// beyond every finite value, a negative zero equals a positive one and is
// false, negating a zero gives the other zero, and `a * b + c` adds `c` to the
// rounded product. The operands of that sum are chosen so the product rounds
// to exactly 1.0 and the sum is 0.0, where one rounding would leave the
// product's lost low bits behind. This case holds Lyra to that, in both
// formats.
module Top;
  real zero = 0.0;
  real one = 1.0;
  real near_above = 1.0000000074505806;
  real near_below = 0.9999999925494194;
  real minus_one = -1.0;
  shortreal short_zero = 0.0;
  shortreal short_one = 1.0;
  shortreal short_near_above = 1.0001220703125;
  shortreal short_near_below = 0.9998779296875;
  shortreal short_minus_one = -1.0;

  real nan;
  real inf;
  real negative_zero;
  real product_then_sum = -1.0;
  shortreal short_nan;
  shortreal short_inf;
  shortreal short_negative_zero;
  shortreal short_product_then_sum = -1.0;

  initial begin
    nan = zero / zero;
    inf = one / zero;
    negative_zero = -zero;
    product_then_sum = near_above * near_below + minus_one;
    short_nan = short_zero / short_zero;
    short_inf = short_one / short_zero;
    short_negative_zero = -short_zero;
    short_product_then_sum =
        short_near_above * short_near_below + short_minus_one;
  end

  final begin
    if ((nan == nan) !== 1'b0) $fatal(1, "a NaN was equal to itself");
    if ((nan != nan) !== 1'b1) $fatal(1, "a NaN was not unequal to itself");
    if ((nan == one) !== 1'b0) $fatal(1, "a NaN was equal to 1.0");
    if ((nan != one) !== 1'b1) $fatal(1, "a NaN was not unequal to 1.0");
    if ((nan < one) !== 1'b0) $fatal(1, "a NaN was less than 1.0");
    if ((nan <= one) !== 1'b0) $fatal(1, "a NaN was at most 1.0");
    if ((nan > one) !== 1'b0) $fatal(1, "a NaN was greater than 1.0");
    if ((nan >= one) !== 1'b0) $fatal(1, "a NaN was at least 1.0");
    if ((one < nan) !== 1'b0) $fatal(1, "1.0 was less than a NaN");
    if ((one >= nan) !== 1'b0) $fatal(1, "1.0 was at least a NaN");
    if (!nan) $fatal(1, "a NaN was false as a condition");

    if (!(inf > 1.0e308)) $fatal(1, "an infinity was not above 1e308");
    if (!(-inf < -1.0e308)) $fatal(1, "minus infinity was not below -1e308");
    if (inf != inf) $fatal(1, "an infinity was unequal to itself");
    if ((inf - inf) == (inf - inf))
      $fatal(1, "infinity minus infinity was equal to itself");

    if (negative_zero != zero) $fatal(1, "-0.0 was unequal to 0.0");
    if (negative_zero < zero) $fatal(1, "-0.0 was less than 0.0");
    if (negative_zero) $fatal(1, "-0.0 was true as a condition");
    if ($realtobits(negative_zero) !== 64'h8000_0000_0000_0000)
      $fatal(1, "negating 0.0 gave the bits %h", $realtobits(negative_zero));
    if ($realtobits(-negative_zero) !== 64'h0)
      $fatal(1, "negating -0.0 gave the bits %h", $realtobits(-negative_zero));

    if (product_then_sum != 0.0)
      $fatal(1, "a * b + c rounded the product with the sum, giving %e",
             product_then_sum);

    if ((short_nan == short_nan) !== 1'b0)
      $fatal(1, "a shortreal NaN was equal to itself");
    if ((short_nan != short_nan) !== 1'b1)
      $fatal(1, "a shortreal NaN was not unequal to itself");
    if ((short_nan < short_one) !== 1'b0)
      $fatal(1, "a shortreal NaN was less than 1.0");
    if ((short_nan >= short_one) !== 1'b0)
      $fatal(1, "a shortreal NaN was at least 1.0");
    if (!short_nan) $fatal(1, "a shortreal NaN was false as a condition");
    if (!(short_inf > short_one))
      $fatal(1, "a shortreal infinity was not above 1.0");
    if (short_negative_zero != short_zero)
      $fatal(1, "a shortreal -0.0 was unequal to 0.0");
    if ($shortrealtobits(short_negative_zero) !== 32'h8000_0000)
      $fatal(1, "negating a shortreal 0.0 gave the bits %h",
             $shortrealtobits(short_negative_zero));
    if (short_product_then_sum != 0.0)
      $fatal(1, "a shortreal a * b + c rounded its product with the sum: %e",
             short_product_then_sum);
    $display("All checks passed");
  end
endmodule

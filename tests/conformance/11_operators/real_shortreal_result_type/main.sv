// In an arithmetic expression whose operands mix the two real types, the result
// is real if any operand is real, so a product involving a real keeps the
// precision of the wider format. Where no operand is real and the value is
// stored in a shortreal, every step is in the narrower format: an
// intermediate is rounded before the next operator reads it, and the stored
// value is still the rounded one when a real is assigned from it
// (LRM 11.3.1).
//
// Whether a real destination widens an operation whose operands are all
// shortreal is not asserted: 11.8.1 types an expression from its operands
// alone, 11.6 sizes one from its context, and no clause says which of the two
// the real family answers to.
module Top;
  real wide = 1.0 + 1.0 / 1073741824.0;
  shortreal short_one = 1.0;
  real real_factor = 2.5;
  shortreal short_factor = 4.0;
  shortreal short_step = 1.0 / 1073741824.0;
  real mixed_product;
  real kept_precision;
  shortreal rounded_intermediate;

  shortreal narrow = 1.0 + 1.0 / 8192.0;
  real narrow_square_expected = 1.0 + 1.0 / 4096.0;
  shortreal narrow_product;
  real narrow_square;

  initial begin
    mixed_product = real_factor * short_factor;
    kept_precision = wide * short_one;

    // A step too small for the narrower format to record beside 1.0, so a sum
    // left in the wider format shows up in the difference. The target starts
    // away from the expected zero, which is also the value it would hold had
    // the assignment never run.
    rounded_intermediate = 1.0;
    rounded_intermediate = (short_one + short_step) - short_one;
    narrow_product = narrow * narrow;
    narrow_square = narrow_product;
  end

  final begin
    if (mixed_product != 10.0)
      $fatal(1, "mixed_product was %g, expected 10.0", mixed_product);
    if (kept_precision != wide)
      $fatal(1, "kept_precision was %.12f, expected %.12f", kept_precision,
             wide);
    if (rounded_intermediate != 0.0)
      $fatal(1, "rounded_intermediate was %g, expected 0",
             rounded_intermediate);
    if (narrow_square != narrow_square_expected)
      $fatal(1, "narrow_square was %.12f, expected %.12f", narrow_square,
             narrow_square_expected);
    $display("All checks passed");
  end
endmodule

// A realtime declaration is treated synonymously with a real declaration and
// the two can be used interchangeably, so a realtime variable holds the same
// double-precision value set, assigns to and from a real with nothing
// converted in either direction, and stands as a real operand in arithmetic
// and in comparison (LRM 6.12, 11.3.1).
module Top;
  realtime sum;
  bit ordered;
  bit reversed;
  real copied_to_real;
  realtime copied_from_real;
  realtime finer_than_a_float;
  bit finer_value_survives;
  real mixed_product;

  initial begin
    realtime low;
    realtime high;
    real wide;

    reversed = 1'b1;

    low = 1.5;
    high = 2.5;
    sum = low + high;
    ordered = (low < high);
    reversed = (low > high);

    copied_to_real = high;
    wide = 3.25;
    copied_from_real = wide;

    // A value that needs more precision than a float carries, so a realtime
    // holding a double is told apart from one narrowed to the shorter format.
    finer_than_a_float = 1.0 + 1.0 / 1073741824.0;
    finer_value_survives = (finer_than_a_float != 1.0);

    mixed_product = wide * low;
  end

  final begin
    if (sum != 4.0) $fatal(1, "sum was %g, expected 4.0", sum);
    if (ordered !== 1'b1)
      $fatal(1, "ordered was %b, expected 1", ordered);
    if (reversed !== 1'b0)
      $fatal(1, "reversed was %b, expected 0", reversed);
    if (copied_to_real != 2.5)
      $fatal(1, "copied_to_real was %g, expected 2.5", copied_to_real);
    if (copied_from_real != 3.25)
      $fatal(1, "copied_from_real was %g, expected 3.25", copied_from_real);
    if (finer_value_survives !== 1'b1)
      $fatal(1, "finer_than_a_float was %.12f, expected it to differ from 1.0",
             finer_than_a_float);
    if (mixed_product != 4.875)
      $fatal(1, "mixed_product was %g, expected 4.875", mixed_product);
    $display("All checks passed");
  end
endmodule

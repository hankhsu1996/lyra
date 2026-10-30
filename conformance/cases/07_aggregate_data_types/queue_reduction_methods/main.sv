// A reduction method combines every element of a queue into one value, and the
// width of that value is the width of the element type unless a with expression
// gives it another, so a sum whose total does not fit the element type wraps
// into it while the same sum taken through a with clause that widens each
// element does not. The iterator's index method reports the position of the
// element the with expression is being evaluated for (LRM 7.12.3, 7.12.4).
module Top;
  byte overflowing [$] = '{100, 100};
  byte narrow_values [$] = '{1, 2, 3, 4};
  int positions [$] = '{4, 1, 3, 2};

  int wrapped_total;
  int widened_total;
  int product_all;
  int xor_all;
  int and_all = -1;
  int or_all;
  int weighted_by_position;

  initial begin
    wrapped_total = overflowing.sum;
    widened_total = overflowing.sum with (int'(item));

    product_all = narrow_values.product;
    xor_all = narrow_values.xor;
    and_all = narrow_values.and;
    or_all = narrow_values.or;

    weighted_by_position = positions.sum with (item * item.index);
  end

  final begin
    if (wrapped_total !== -56)
      $fatal(1, "wrapped_total was %0d, expected -56", wrapped_total);
    if (widened_total !== 200)
      $fatal(1, "widened_total was %0d, expected 200", widened_total);
    if (product_all !== 24)
      $fatal(1, "product_all was %0d, expected 24", product_all);
    if (xor_all !== 4) $fatal(1, "xor_all was %0d, expected 4", xor_all);
    if (and_all !== 0) $fatal(1, "and_all was %0d, expected 0", and_all);
    if (or_all !== 7) $fatal(1, "or_all was %0d, expected 7", or_all);
    if (weighted_by_position !== 13)
      $fatal(1, "weighted_by_position was %0d, expected 13",
             weighted_by_position);
    $display("All checks passed");
  end
endmodule

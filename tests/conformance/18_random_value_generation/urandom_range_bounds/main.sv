// $urandom_range returns an unsigned value inside the range its two arguments
// describe. An omitted second argument is zero, and a maxval smaller than
// minval reverses the two, so all three spellings of the same range produce a
// value in it; a range whose bounds are equal admits only that one value
// (LRM 18.13.2).
module Top;
  int unsigned ascending;
  int unsigned descending;
  int unsigned implicit_zero;
  int unsigned single_value;

  initial begin
    ascending = 99;
    descending = 99;
    implicit_zero = 99;
    single_value = 99;

    ascending = $urandom_range(0, 7);
    descending = $urandom_range(7, 0);
    implicit_zero = $urandom_range(7);
    single_value = $urandom_range(3, 3);
  end

  final begin
    if (ascending > 7)
      $fatal(1, "$urandom_range(0, 7) was %0d, expected 0 through 7", ascending);
    if (descending > 7)
      $fatal(1, "$urandom_range(7, 0) was %0d, expected 0 through 7", descending);
    if (implicit_zero > 7)
      $fatal(1, "$urandom_range(7) was %0d, expected 0 through 7", implicit_zero);
    if (single_value !== 3)
      $fatal(1, "$urandom_range(3, 3) was %0d, expected 3", single_value);
    $display("All checks passed");
  end
endmodule

// $clog2 returns the ceiling of the log base 2 of its argument. The argument
// is treated as an unsigned value, so a negative operand counts as the
// unsigned value its bits spell, and an argument of 0 produces 0. None of that
// depends on whether the argument is a constant or a value the simulation
// computes (LRM 20.8.1).
module Top;
  parameter int unsigned Bytes = 8;

  int folded_power_of_two;
  int folded_zero;
  int folded_ceiling;

  int width;
  int computed_one;
  int computed_seven;
  int computed_eight;
  int computed_zero;
  int computed_wide;
  int computed_negative;

  initial begin
    folded_power_of_two = $clog2(Bytes);
    folded_zero = $clog2(0);
    folded_ceiling = $clog2(7);

    width = 1;
    computed_one = $clog2(width);
    width = 7;
    computed_seven = $clog2(width);
    width = 8;
    computed_eight = $clog2(width);
    width = 0;
    computed_zero = $clog2(width);
    width = 1 << 20;
    computed_wide = $clog2(width);
    width = -1;
    computed_negative = $clog2(width);
  end

  final begin
    if (folded_power_of_two !== 3)
      $fatal(1, "$clog2(8) was %0d, expected 3", folded_power_of_two);
    if (folded_zero !== 0)
      $fatal(1, "$clog2(0) was %0d, expected 0", folded_zero);
    if (folded_ceiling !== 3)
      $fatal(1, "$clog2(7) was %0d, expected 3", folded_ceiling);
    if (computed_one !== 0)
      $fatal(1, "$clog2(1) was %0d, expected 0", computed_one);
    if (computed_seven !== 3)
      $fatal(1, "$clog2(7) was %0d, expected 3", computed_seven);
    if (computed_eight !== 3)
      $fatal(1, "$clog2(8) was %0d, expected 3", computed_eight);
    if (computed_zero !== 0)
      $fatal(1, "$clog2(0) was %0d, expected 0", computed_zero);
    if (computed_wide !== 20)
      $fatal(1, "$clog2(1048576) was %0d, expected 20", computed_wide);
    if (computed_negative !== 32)
      $fatal(1, "$clog2 of an all-ones operand was %0d, expected 32",
             computed_negative);
    $display("All checks passed");
  end
endmodule

// $rtoi converts a real to an integer by truncating it, which is what
// separates it from casting the same value to an integral type: the cast
// rounds, so 3.7 is 4 through a cast and 3 through $rtoi, and truncation drops
// the fraction rather than moving toward the next value down, so -3.7 is -3
// rather than -4. $itor converts the other way and keeps the integer's value
// (LRM 20.5).
module Top;
  real frac_above = 3.7;
  real neg_frac_above = -3.7;
  real lrm_example = 123.45;
  int whole = 123;

  integer truncated_up = -1;
  integer truncated_down = -1;
  integer truncated_example = -1;
  int rounded_up = -1;
  real widened = -1.0;

  initial begin
    truncated_up = $rtoi(frac_above);
    truncated_down = $rtoi(neg_frac_above);
    truncated_example = $rtoi(lrm_example);
    rounded_up = int'(frac_above);
    widened = $itor(whole);
  end

  final begin
    if (truncated_up !== 3)
      $fatal(1, "$rtoi(3.7) was %0d, expected 3", truncated_up);
    if (truncated_down !== -3)
      $fatal(1, "$rtoi(-3.7) was %0d, expected -3", truncated_down);
    if (truncated_example !== 123)
      $fatal(1, "$rtoi(123.45) was %0d, expected 123", truncated_example);
    if (rounded_up !== 4)
      $fatal(1, "a cast of 3.7 was %0d, expected 4", rounded_up);
    if (widened != 123.0)
      $fatal(1, "$itor(123) was %g, expected 123.0", widened);
    $display("All checks passed");
  end
endmodule

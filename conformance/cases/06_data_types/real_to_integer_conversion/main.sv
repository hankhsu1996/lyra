// A real number assigned to an integer is converted by rounding to the nearest
// integer rather than by truncating toward zero, and a fractional part of
// exactly 0.5 is rounded away from zero rather than toward an even result. So
// 3.7 becomes 4 and 3.2 becomes 3, while 0.5 becomes 1, 2.5 becomes 3, 35.5
// becomes 36, -1.5 becomes -2 and -2.5 becomes -3 (LRM 6.12.1).
module Top;
  real frac_below = 3.2;
  real frac_above = 3.7;
  real neg_frac_below = -3.2;
  real neg_frac_above = -3.7;
  real tie_low = 0.5;
  real tie_mid = 2.5;
  real tie_high = 35.5;
  real neg_tie_low = -1.5;
  real neg_tie_high = -2.5;
  int i_frac_below;
  int i_frac_above;
  int i_neg_frac_below;
  int i_neg_frac_above;
  int i_tie_low;
  int i_tie_mid;
  int i_tie_high;
  int i_neg_tie_low;
  int i_neg_tie_high;

  initial begin
    i_frac_below = frac_below;
    i_frac_above = frac_above;
    i_neg_frac_below = neg_frac_below;
    i_neg_frac_above = neg_frac_above;
    i_tie_low = tie_low;
    i_tie_mid = tie_mid;
    i_tie_high = tie_high;
    i_neg_tie_low = neg_tie_low;
    i_neg_tie_high = neg_tie_high;
  end

  final begin
    if (i_frac_below !== 3)
      $fatal(1, "i_frac_below was %0d, expected 3", i_frac_below);
    if (i_frac_above !== 4)
      $fatal(1, "i_frac_above was %0d, expected 4", i_frac_above);
    if (i_neg_frac_below !== -3)
      $fatal(1, "i_neg_frac_below was %0d, expected -3", i_neg_frac_below);
    if (i_neg_frac_above !== -4)
      $fatal(1, "i_neg_frac_above was %0d, expected -4", i_neg_frac_above);
    if (i_tie_low !== 1)
      $fatal(1, "i_tie_low was %0d, expected 1", i_tie_low);
    if (i_tie_mid !== 3)
      $fatal(1, "i_tie_mid was %0d, expected 3", i_tie_mid);
    if (i_tie_high !== 36)
      $fatal(1, "i_tie_high was %0d, expected 36", i_tie_high);
    if (i_neg_tie_low !== -2)
      $fatal(1, "i_neg_tie_low was %0d, expected -2", i_neg_tie_low);
    if (i_neg_tie_high !== -3)
      $fatal(1, "i_neg_tie_high was %0d, expected -3", i_neg_tie_high);
    $display("All checks passed");
  end
endmodule

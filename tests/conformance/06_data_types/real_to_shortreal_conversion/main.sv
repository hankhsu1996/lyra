// A real is a C double and a shortreal a C float, both represented as IEEE Std
// 754 describes, so a real value assigned to a shortreal is rounded to the
// narrower format. A value the narrower format can hold exactly survives the
// round trip unchanged, while one that needs more precision comes back
// changed, by no more than half a step of the narrower format (LRM 6.12).
module Top;
  real exact_source = 2.5;
  real near_one = 1.0 + 1.0 / 1073741824.0;
  real pi_source = 3.14159265358979;
  shortreal exact_narrow;
  shortreal near_one_narrow;
  shortreal pi_narrow;
  real exact_back;
  real near_one_back;
  real pi_back;
  real pi_error;

  initial begin
    exact_narrow = exact_source;
    exact_back = exact_narrow;

    near_one_narrow = near_one;
    near_one_back = near_one_narrow;

    pi_narrow = pi_source;
    pi_back = pi_narrow;
    pi_error = pi_back - pi_source;
    if (pi_error < 0.0) pi_error = -pi_error;
  end

  final begin
    if (exact_back != 2.5)
      $fatal(1, "exact_back was %g, expected 2.5", exact_back);
    if (near_one_back != 1.0)
      $fatal(1, "near_one_back was %.12f, expected 1.0", near_one_back);
    if (pi_error <= 0.0)
      $fatal(1, "pi_error was %g, expected a nonzero rounding error",
             pi_error);
    if (pi_error > 0.0000002)
      $fatal(1, "pi_error was %g, expected at most 0.0000002", pi_error);
    $display("All checks passed");
  end
endmodule

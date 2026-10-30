// When either operand of the power operator is real the result is real, so a
// fractional exponent gives a root and a negative exponent gives a reciprocal.
// A negative base is defined when the exponent has an integral value. The
// exponent is self-determined, so an integral exponent expression is evaluated
// with integer division before it reaches the operator, and the unary minus
// binds tighter than **, so -3.0 ** 2.0 raises -3.0 to the power 2.0
// (LRM 11.3.1, 11.3.2, 11.4.3, Table 11-5).
module Top;
  real two = 2.0;
  real three = 3.0;
  real nine = 9.0;
  real half = 0.5;
  int minus_one = -1;
  real cube;
  real root;
  real negative_base;
  real reciprocal;
  real truncated_exponent;

  initial begin
    cube = two ** three;
    root = nine ** half;
    negative_base = -three ** two;
    reciprocal = two ** minus_one;
    truncated_exponent = nine ** (1 / 2);
  end

  final begin
    if (cube != 8.0) $fatal(1, "cube was %g, expected 8.0", cube);
    if (root != 3.0) $fatal(1, "root was %g, expected 3.0", root);
    if (negative_base != 9.0)
      $fatal(1, "negative_base was %g, expected 9.0", negative_base);
    if (reciprocal != 0.5)
      $fatal(1, "reciprocal was %g, expected 0.5", reciprocal);
    if (truncated_exponent != 1.0)
      $fatal(1, "truncated_exponent was %g, expected 1.0",
             truncated_exponent);
    $display("All checks passed");
  end
endmodule

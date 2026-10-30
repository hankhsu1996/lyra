// The binary arithmetic operators + - * / and the unary + and - accept real
// operands, and whenever an operand of one of them is real the result is real.
// A real quotient therefore keeps the fractional part that integer division
// would truncate toward zero, and an integral operand is converted to an
// equivalent real value (LRM 11.3.1, 11.4.3, Table 11-1, Table 11-6).
module Top;
  real addend = 1.5;
  real other_addend = 2.5;
  real minuend = 5.5;
  real subtrahend = 2.0;
  real multiplicand = 2.5;
  real multiplier = 4.0;
  real dividend = 7.5;
  real divisor = 2.5;
  real fractional_dividend = 5.0;
  real fractional_divisor = 2.0;
  real to_negate = 5.5;
  real group_first = 2.0;
  real group_second = 3.0;
  real group_scale = 4.0;
  int integral_operand = 2;
  real sum;
  real difference;
  real product;
  real quotient;
  real fractional_quotient;
  real negated;
  real plussed;
  real grouped;
  real with_integral;

  initial begin
    sum = addend + other_addend;
    difference = minuend - subtrahend;
    product = multiplicand * multiplier;
    quotient = dividend / divisor;
    fractional_quotient = fractional_dividend / fractional_divisor;
    negated = -to_negate;
    plussed = +to_negate;
    grouped = (group_first + group_second) * group_scale;
    with_integral = multiplicand * integral_operand;
  end

  final begin
    if (sum != 4.0) $fatal(1, "sum was %g, expected 4.0", sum);
    if (difference != 3.5)
      $fatal(1, "difference was %g, expected 3.5", difference);
    if (product != 10.0) $fatal(1, "product was %g, expected 10.0", product);
    if (quotient != 3.0)
      $fatal(1, "quotient was %g, expected 3.0", quotient);
    if (fractional_quotient != 2.5)
      $fatal(1, "fractional_quotient was %g, expected 2.5",
             fractional_quotient);
    if (negated != -5.5) $fatal(1, "negated was %g, expected -5.5", negated);
    if (plussed != 5.5) $fatal(1, "plussed was %g, expected 5.5", plussed);
    if (grouped != 20.0) $fatal(1, "grouped was %g, expected 20.0", grouped);
    if (with_integral != 5.0)
      $fatal(1, "with_integral was %g, expected 5.0", with_integral);
    $display("All checks passed");
  end
endmodule

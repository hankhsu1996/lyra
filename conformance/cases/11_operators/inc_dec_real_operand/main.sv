// Applied to a real operand, the increment and decrement operators change it
// by 1.0. The prefix and postfix forms still differ in which value the
// expression yields (LRM 11.4.2, Table 11-1).
module Top;
  real operand;
  real prefix_value;
  real after_prefix;
  real postfix_value;
  real after_postfix;
  real fractional;
  real after_statement;

  initial begin
    operand = 2.5;
    prefix_value = ++operand;
    after_prefix = operand;
    postfix_value = operand--;
    after_postfix = operand;

    fractional = -0.25;
    fractional++;
    after_statement = fractional;
  end

  final begin
    if (prefix_value != 3.5)
      $fatal(1, "++r yielded %f, expected 3.5", prefix_value);
    if (after_prefix != 3.5)
      $fatal(1, "++r left r at %f, expected 3.5", after_prefix);
    if (postfix_value != 3.5)
      $fatal(1, "r-- yielded %f, expected 3.5", postfix_value);
    if (after_postfix != 2.5)
      $fatal(1, "r-- left r at %f, expected 2.5", after_postfix);
    if (after_statement != 0.75)
      $fatal(1, "-0.25 incremented to %f, expected 0.75", after_statement);
    $display("All checks passed");
  end
endmodule

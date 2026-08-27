// A string variable holds an ordered collection of characters. Its declaration
// may carry an initial value -- a string literal, the value "" for an empty
// string, or an expression of string type -- and one declared without an
// initial value is initialized to "", the empty string, whose length is zero.
// An unassigned string is therefore an ordinary operand and contributes no
// characters to a concatenation (LRM 6.16, 6.16.1).
module Top;
  parameter string default_name = "John Smith";

  string from_literal = "hello";
  string from_parameter = default_name;
  string from_empty_literal = "";
  string without_initial_value;

  int literal_len;
  int parameter_len;
  int empty_literal_len = -1;
  int without_initial_value_len = -1;
  string around_unassigned;

  initial begin
    literal_len = from_literal.len();
    parameter_len = from_parameter.len();
    empty_literal_len = from_empty_literal.len();
    without_initial_value_len = without_initial_value.len();
    around_unassigned = {"a", without_initial_value, "b"};
  end

  final begin
    if (from_literal != "hello")
      $fatal(1, "from_literal was \"%s\", expected \"hello\"", from_literal);
    if (literal_len !== 5)
      $fatal(1, "literal_len was %0d, expected 5", literal_len);

    if (from_parameter != "John Smith")
      $fatal(1, "from_parameter was \"%s\", expected \"John Smith\"",
             from_parameter);
    if (parameter_len !== 10)
      $fatal(1, "parameter_len was %0d, expected 10", parameter_len);

    if (from_empty_literal != "")
      $fatal(1, "from_empty_literal was \"%s\", expected \"\"",
             from_empty_literal);
    if (empty_literal_len !== 0)
      $fatal(1, "empty_literal_len was %0d, expected 0", empty_literal_len);

    if (without_initial_value != "")
      $fatal(1, "without_initial_value was \"%s\", expected \"\"",
             without_initial_value);
    if (without_initial_value_len !== 0)
      $fatal(1, "without_initial_value_len was %0d, expected 0",
             without_initial_value_len);

    if (around_unassigned != "ab")
      $fatal(1, "around_unassigned was \"%s\", expected \"ab\"",
             around_unassigned);
    $display("All checks passed");
  end
endmodule

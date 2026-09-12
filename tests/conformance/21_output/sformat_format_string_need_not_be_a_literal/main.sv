// The format argument of $sformat is any expression of string, integral, or
// unpacked-array-of-byte type, and its content is read as the format string,
// so it need not be a literal whose value is known when the design is
// compiled. Supplying more arguments than the format has conversions is not
// fatal either: the surplus goes unused and execution continues
// (LRM 21.3.3). An argument is an expression the call evaluates once, whatever
// the format turns out to ask of it.
module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;

  string format_text;
  byte format_bytes[0:3];
  int count;
  string name;
  int evaluations;

  string from_a_variable;
  string from_an_expression;
  string from_bytes;
  string with_a_string_argument;
  string with_surplus_arguments;
  string from_a_called_argument;

  function automatic pair_t Counted();
    evaluations = evaluations + 1;
    Counted = '{a: evaluations, b: 0};
  endfunction

  initial begin
    count = 42;
    name = "lyra";
    evaluations = 0;

    format_text = "a=%0d";
    $sformat(from_a_variable, format_text, count);
    $sformat(from_an_expression, {"<", format_text, ">"}, count);

    format_text = "%s=%0d";
    $sformat(with_a_string_argument, format_text, name, count);

    format_bytes[0] = "h";
    format_bytes[1] = "=";
    format_bytes[2] = "%";
    format_bytes[3] = "h";
    $sformat(from_bytes, format_bytes, count);

    format_text = "only=%0d";
    $sformat(with_surplus_arguments, format_text, count, count);

    format_text = "%p";
    $sformat(from_a_called_argument, format_text, Counted());
  end

  final begin
    if (from_a_variable != "a=42")
      $fatal(1, "a format string held in a variable gave '%s', expected a=42",
             from_a_variable);
    if (from_an_expression != "<a=42>")
      $fatal(1, "a concatenated format gave '%s', expected <a=42>",
             from_an_expression);
    if (with_a_string_argument != "lyra=42")
      $fatal(1, "a string conversion gave '%s', expected lyra=42",
             with_a_string_argument);
    if (from_bytes != "h=0000002a")
      $fatal(1, "a byte-array format gave '%s', expected h=0000002a",
             from_bytes);
    if (with_surplus_arguments != "only=42")
      $fatal(1, "a surplus argument gave '%s', expected only=42",
             with_surplus_arguments);
    if (evaluations != 1)
      $fatal(1, "an argument was evaluated %0d times, expected 1", evaluations);
    if (from_a_called_argument != "'{a:1, b:0}")
      $fatal(1, "a called argument gave '%s', expected '{a:1, b:0}",
             from_a_called_argument);
    $display("All checks passed");
  end
endmodule

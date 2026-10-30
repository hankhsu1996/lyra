// A display task's arguments contribute to its text in the order they are
// written, with nothing between them. Every string literal among them is output
// literally, and each format specification in one formats an expression after
// it; an expression no specification took is displayed in the task's default
// radix; an empty argument produces a single space (LRM 21.2.1, 21.2.1.1).
// $swrite takes the same list and hands back the text (LRM 21.3.3), which is
// how the text is read here. $sformat differs in reading its one format
// argument as a format string and no other (LRM 21.3.3).
module Top;
  int five;
  logic [7:0] byte_value;

  string two_literals;
  string literal_after_value;
  string later_literal_formats;
  string literal_taken_by_specification;
  string adjacent_values;
  string empty_between;
  string empty_first_and_last;
  string only_empty;
  string hexadecimal_default;
  string formatted_once;

  initial begin
    five = 5;
    byte_value = 8'hA7;

    two_literals = "unset";
    $swrite(two_literals, "left", "right");

    literal_after_value = "unset";
    $swrite(literal_after_value, "a=%0d", five, " b=%0d", five, "!");

    later_literal_formats = "unset";
    $swriteh(later_literal_formats, byte_value, " as decimal %0d", byte_value);

    literal_taken_by_specification = "unset";
    $swrite(literal_taken_by_specification, "<%s>", "%0d", "<");

    adjacent_values = "unset";
    $swriteh(adjacent_values, byte_value, byte_value);

    empty_between = "unset";
    $swrite(empty_between, "b",, "c");

    empty_first_and_last = "unset";
    $swrite(empty_first_and_last,, "d",);

    only_empty = "unset";
    $swrite(only_empty,,);

    hexadecimal_default = "unset";
    $swriteh(hexadecimal_default, "x", byte_value, "y");

    formatted_once = "unset";
    $sformat(formatted_once, "%s|%s", "%0d", "tail");
  end

  final begin
    if (two_literals != "leftright")
      $fatal(1, "two literals gave '%s', expected 'leftright'", two_literals);
    if (literal_after_value != "a=5 b=5!")
      $fatal(1, "a literal after a value gave '%s', expected 'a=5 b=5!'",
             literal_after_value);
    if (later_literal_formats != "a7 as decimal 167")
      $fatal(1, "a later literal's specification gave '%s'",
             later_literal_formats);
    if (literal_taken_by_specification != "<%0d><")
      $fatal(1, "a literal a specification took gave '%s', expected '<%%0d><'",
             literal_taken_by_specification);
    if (adjacent_values != "a7a7")
      $fatal(1, "two adjacent values gave '%s', expected 'a7a7'",
             adjacent_values);
    if (empty_between != "b c")
      $fatal(1, "an empty argument between two gave '%s', expected 'b c'",
             empty_between);
    if (empty_first_and_last != " d ")
      $fatal(1, "empty arguments around one gave '%s', expected ' d '",
             empty_first_and_last);
    if (only_empty != "  ")
      $fatal(1, "a single comma gave '%s', expected two spaces", only_empty);
    if (hexadecimal_default != "xa7y")
      $fatal(1, "a value between literals gave '%s', expected 'xa7y'",
             hexadecimal_default);
    if (formatted_once != "%0d|tail")
      $fatal(1, "$sformat gave '%s', expected '%%0d|tail'", formatted_once);
    $display("All checks passed");
  end
endmodule

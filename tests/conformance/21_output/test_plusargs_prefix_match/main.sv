// @argv: +HELLO
//
// $test$plusargs searches the plusargs given on the simulation command line
// for the string it is passed, which does not carry the leading plus sign. It
// returns a nonzero integer when the prefix of one of those plusargs matches
// every character of that string, and zero when none does. A string shorter
// than a plusarg therefore matches it, a string longer than one does not, and
// the match runs from the plusarg's first character rather than from anywhere
// within it. The string need not be written as a literal; it may reach the
// call in a variable (LRM 21.6).
module Top;
  int whole;
  int shorter;
  int shortest;
  int longer;
  int diverges_early;
  int inside_but_not_prefix;
  int from_variable;

  string held;

  initial begin
    longer = 1;
    diverges_early = 1;
    inside_but_not_prefix = 1;

    whole = $test$plusargs("HELLO");
    shorter = $test$plusargs("HE");
    shortest = $test$plusargs("H");
    longer = $test$plusargs("HELLO_HERE");
    diverges_early = $test$plusargs("HI");
    inside_but_not_prefix = $test$plusargs("LO");

    held = "HELLO";
    from_variable = $test$plusargs(held);
  end

  final begin
    if (whole === 0)
      $fatal(1, "HELLO against +HELLO returned 0, expected nonzero");
    if (shorter === 0)
      $fatal(1, "HE against +HELLO returned 0, expected nonzero");
    if (shortest === 0)
      $fatal(1, "H against +HELLO returned 0, expected nonzero");
    if (longer !== 0)
      $fatal(1, "HELLO_HERE against +HELLO returned %0d, expected 0", longer);
    if (diverges_early !== 0)
      $fatal(1, "HI against +HELLO returned %0d, expected 0", diverges_early);
    if (inside_but_not_prefix !== 0)
      $fatal(1, "LO against +HELLO returned %0d, expected 0",
             inside_but_not_prefix);
    if (from_variable === 0)
      $fatal(1, "a variable holding HELLO returned 0, expected nonzero");
    $display("All checks passed");
  end
endmodule

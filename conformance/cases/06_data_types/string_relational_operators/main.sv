// The relational operators < <= > >= order two strings lexicographically, by
// the same comparison the compare method makes, and yield 1 when the relation
// holds and 0 when it does not. A string that is a prefix of another orders
// before it, which puts the empty string before every other string, and either
// operand may be a string literal (LRM 6.16, 6.16.6, Table 6-9).
module Top;
  string abc = "abc";
  string abc_again = "abc";
  string abd = "abd";
  string ab = "ab";
  string empty = "";
  string x = "x";

  bit differing_less;
  bit differing_less_reversed;
  bit differing_greater;
  bit prefix_less;
  bit prefix_greater;
  bit empty_less;
  bit empty_greater;
  bit equal_less;
  bit equal_less_equal;
  bit equal_greater_equal;
  bit literal_greater_equal;
  bit literal_less;

  initial begin
    // Each target a check expects at 0 starts at 1, so a comparison that
    // never ran cannot pass for one that answered 0.
    differing_less_reversed = 1'b1;
    empty_greater = 1'b1;
    equal_less = 1'b1;

    differing_less = (abc < abd);
    differing_less_reversed = (abd < abc);
    differing_greater = (abd > abc);
    prefix_less = (ab < abc);
    prefix_greater = (abc > ab);
    empty_less = (empty < x);
    empty_greater = (empty > x);
    equal_less = (abc < abc_again);
    equal_less_equal = (abc <= abc_again);
    equal_greater_equal = (abc >= abc_again);
    literal_greater_equal = (x >= "x");
    literal_less = (x < "y");
  end

  final begin
    if (differing_less !== 1'b1)
      $fatal(1, "differing_less was %b, expected 1", differing_less);
    if (differing_less_reversed !== 1'b0)
      $fatal(1, "differing_less_reversed was %b, expected 0",
             differing_less_reversed);
    if (differing_greater !== 1'b1)
      $fatal(1, "differing_greater was %b, expected 1", differing_greater);
    if (prefix_less !== 1'b1)
      $fatal(1, "prefix_less was %b, expected 1", prefix_less);
    if (prefix_greater !== 1'b1)
      $fatal(1, "prefix_greater was %b, expected 1", prefix_greater);
    if (empty_less !== 1'b1)
      $fatal(1, "empty_less was %b, expected 1", empty_less);
    if (empty_greater !== 1'b0)
      $fatal(1, "empty_greater was %b, expected 0", empty_greater);
    if (equal_less !== 1'b0)
      $fatal(1, "equal_less was %b, expected 0", equal_less);
    if (equal_less_equal !== 1'b1)
      $fatal(1, "equal_less_equal was %b, expected 1", equal_less_equal);
    if (equal_greater_equal !== 1'b1)
      $fatal(1, "equal_greater_equal was %b, expected 1", equal_greater_equal);
    if (literal_greater_equal !== 1'b1)
      $fatal(1, "literal_greater_equal was %b, expected 1",
             literal_greater_equal);
    if (literal_less !== 1'b1)
      $fatal(1, "literal_less was %b, expected 1", literal_less);
    $display("All checks passed");
  end
endmodule

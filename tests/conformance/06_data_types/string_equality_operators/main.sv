// Two strings are equal when they hold the same characters, so == yields 1
// then and 0 otherwise, and != is its logical negation. Either operand may be
// a string literal, which is converted to string type for the comparison. The
// comparison is over characters, so it is case sensitive, and an unassigned
// string equals the empty string (LRM 6.16, Table 6-9).
module Top;
  string greeting = "hi";
  string same = "hi";
  string different = "bye";
  string capitalized = "Hi";
  string empty = "";
  string never_assigned;

  bit equal_vars;
  bit equal_literal;
  bit equal_different;
  bit equal_case;
  bit not_equal_different;
  bit not_equal_same;
  bit empty_equals_unassigned;
  int taken_branch;

  initial begin
    // Each target a check expects at 0 starts at 1, so a comparison that
    // never ran cannot pass for one that answered 0.
    equal_different = 1'b1;
    equal_case = 1'b1;
    not_equal_same = 1'b1;

    equal_vars = (greeting == same);
    equal_literal = (greeting == "hi");
    equal_different = (greeting == different);
    equal_case = (greeting == capitalized);
    not_equal_different = (greeting != different);
    not_equal_same = (greeting != same);
    empty_equals_unassigned = (empty == never_assigned);

    if (greeting == "hi") taken_branch = 1;
    else taken_branch = 2;
  end

  final begin
    if (equal_vars !== 1'b1)
      $fatal(1, "equal_vars was %b, expected 1", equal_vars);
    if (equal_literal !== 1'b1)
      $fatal(1, "equal_literal was %b, expected 1", equal_literal);
    if (equal_different !== 1'b0)
      $fatal(1, "equal_different was %b, expected 0", equal_different);
    if (equal_case !== 1'b0)
      $fatal(1, "equal_case was %b, expected 0", equal_case);
    if (not_equal_different !== 1'b1)
      $fatal(1, "not_equal_different was %b, expected 1", not_equal_different);
    if (not_equal_same !== 1'b0)
      $fatal(1, "not_equal_same was %b, expected 0", not_equal_same);
    if (empty_equals_unassigned !== 1'b1)
      $fatal(1, "empty_equals_unassigned was %b, expected 1",
             empty_equals_unassigned);
    if (taken_branch !== 1)
      $fatal(1, "taken_branch was %0d, expected 1", taken_branch);
    $display("All checks passed");
  end
endmodule

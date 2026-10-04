// The expression on the left-hand side of the set membership operator is one
// operand, compared with each member of the set until a match is found (LRM
// 11.4.13). It is written once, so a function called in it runs once however
// many members the set has and whichever of them matches -- for a set of
// values, a set holding a range, a set none of whose members match, and where
// the operator is itself an operand.
module Top;
  int calls;

  function automatic int counted(int value);
    calls = calls + 1;
    return value;
  endfunction

  bit in_values;
  bit in_range;
  bit in_none;
  bit as_operand;

  int on_values;
  int on_range;
  int on_none;
  int on_operand;
  int on_skipped;
  bit in_skipped;
  bit take_the_test;

  initial begin
    calls = 0;
    in_values = counted(3) inside {1, 2, 3, 4};
    on_values = calls;

    calls = 0;
    in_range = counted(7) inside {1, [5:9], 20};
    on_range = calls;

    calls = 0;
    in_none = counted(50) inside {1, [5:9], 20, 30, 40};
    on_none = calls;

    calls = 0;
    as_operand = !(counted(2) inside {1, 2, 3}) || in_values;
    on_operand = calls;

    // The operand is evaluated where the operator is written, so the operator
    // in an operand the run does not take evaluates nothing (LRM 11.3.5).
    calls = 0;
    in_skipped = take_the_test ? (counted(2) inside {1, 2, 3}) : 1'b0;
    on_skipped = calls;
  end

  final begin
    if (in_values !== 1'b1) $fatal(1, "3 was not found in {1, 2, 3, 4}");
    if (in_range !== 1'b1) $fatal(1, "7 was not found in {1, [5:9], 20}");
    if (in_none !== 1'b0) $fatal(1, "50 was found in a set that lacks it");
    if (as_operand !== 1'b1) $fatal(1, "the operator as an operand was wrong");

    if (on_values !== 1)
      $fatal(1, "a set of values ran the left operand %0d times, expected 1",
             on_values);
    if (on_range !== 1)
      $fatal(1, "a set with a range ran the left operand %0d times, expected 1",
             on_range);
    if (on_none !== 1)
      $fatal(1, "a set with no match ran the left operand %0d times, expected 1",
             on_none);
    if (on_operand !== 1)
      $fatal(1, "as an operand it ran the left operand %0d times, expected 1",
             on_operand);
    if (on_skipped !== 0)
      $fatal(1, "in an untaken arm it ran the left operand %0d times, expected 0",
             on_skipped);
    if (in_skipped !== 1'b0) $fatal(1, "the untaken arm's result was wrong");
    $display("All checks passed");
  end
endmodule

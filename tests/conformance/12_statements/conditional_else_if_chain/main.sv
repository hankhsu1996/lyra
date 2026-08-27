// An if-else-if chain evaluates its predicates in the order they are written
// and executes the statement of the first true one, which ends the chain, so
// a later predicate that is also true has no effect. A predicate that is
// false, or that is x or z, passes on to the next. The trailing else covers
// the case where none of them is true and may be left out, and then a chain
// that matches nothing executes nothing (LRM 12.4.1, 12.4).
module Top;
  int first_true_wins;
  int middle_branch;
  int trailing_else;
  int no_trailing_else;
  int unknown_falls_through;

  initial begin
    int flag_a;
    int flag_b;
    int flag_c;
    logic unknown;

    // Every predicate holds, so only the first one's statement runs.
    first_true_wins = 0;
    if (1) first_true_wins = 1;
    else if (1) first_true_wins = 2;
    else if (1) first_true_wins = 3;
    else first_true_wins = 4;

    flag_a = 0;
    flag_b = 1;
    flag_c = 1;
    if (flag_a) middle_branch = 10;
    else if (flag_b) middle_branch = 22;
    else if (flag_c) middle_branch = 30;
    else middle_branch = 40;

    flag_b = 0;
    flag_c = 0;
    if (flag_a) trailing_else = 10;
    else if (flag_b) trailing_else = 20;
    else trailing_else = 33;

    no_trailing_else = 0;
    if (flag_a) no_trailing_else = 10;
    else if (flag_b) no_trailing_else = 20;

    unknown = 1'bx;
    if (unknown) unknown_falls_through = 1;
    else if (1) unknown_falls_through = 2;
    else unknown_falls_through = 3;
  end

  final begin
    if (first_true_wins !== 1)
      $fatal(1, "first_true_wins was %0d, expected 1", first_true_wins);
    if (middle_branch !== 22)
      $fatal(1, "middle_branch was %0d, expected 22", middle_branch);
    if (trailing_else !== 33)
      $fatal(1, "trailing_else was %0d, expected 33", trailing_else);
    if (no_trailing_else !== 0)
      $fatal(1, "no_trailing_else was %0d, expected 0", no_trailing_else);
    if (unknown_falls_through !== 2)
      $fatal(1, "unknown_falls_through was %0d, expected 2",
             unknown_falls_through);
    $display("All checks passed");
  end
endmodule

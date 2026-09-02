// How many arms a qualifier evaluates follows from what it asserts, so the two
// qualifier families reach opposite answers on the same chain. An if-else-if
// construct evaluates its expressions in order and the first true one
// terminates the whole chain, and a case statement's linear search terminates
// at the first matching item; priority states the order those already have and
// overrides neither. Unique and unique0 do override them -- checking that at
// most one arm holds cannot stop at the first that does -- so the standard has
// them continue evaluating and comparing after a match. Each arm here reports
// its own evaluation, so the count is what separates the two, and the arm that
// runs is the first one either way (LRM 12.4.1, 12.4.2, 12.5, 12.5.3).
module Top;
  int evaluated;
  int priority_if_count;
  int unique_if_count;
  int priority_case_count;
  int unique_case_count;
  int priority_if_taken;
  int unique_if_taken;
  int priority_case_taken;
  int unique_case_taken;

  function automatic bit Holds(input bit answer);
    evaluated = evaluated + 1;
    return answer;
  endfunction

  initial begin
    evaluated = 0;
    priority_if_taken = 0;
    priority if (Holds(1'b1)) priority_if_taken = 1;
    else if (Holds(1'b0)) priority_if_taken = 2;
    else if (Holds(1'b0)) priority_if_taken = 3;
    priority_if_count = evaluated;

    evaluated = 0;
    unique_if_taken = 0;
    unique if (Holds(1'b1)) unique_if_taken = 1;
    else if (Holds(1'b0)) unique_if_taken = 2;
    else if (Holds(1'b0)) unique_if_taken = 3;
    unique_if_count = evaluated;

    evaluated = 0;
    priority_case_taken = 0;
    priority case (1'b1)
      Holds(1'b1): priority_case_taken = 1;
      Holds(1'b0): priority_case_taken = 2;
      Holds(1'b0): priority_case_taken = 3;
    endcase
    priority_case_count = evaluated;

    evaluated = 0;
    unique_case_taken = 0;
    unique case (1'b1)
      Holds(1'b1): unique_case_taken = 1;
      Holds(1'b0): unique_case_taken = 2;
      Holds(1'b0): unique_case_taken = 3;
    endcase
    unique_case_count = evaluated;
  end

  final begin
    if (priority_if_count !== 1)
      $fatal(
          1, "priority if evaluated %0d conditions, expected 1",
          priority_if_count);
    if (unique_if_count !== 3)
      $fatal(
          1, "unique if evaluated %0d conditions, expected 3", unique_if_count);
    if (priority_case_count !== 1)
      $fatal(
          1, "priority case evaluated %0d case items, expected 1",
          priority_case_count);
    if (unique_case_count !== 3)
      $fatal(
          1, "unique case evaluated %0d case items, expected 3",
          unique_case_count);
    if (priority_if_taken !== 1)
      $fatal(
          1, "priority_if_taken was %0d, expected 1", priority_if_taken);
    if (unique_if_taken !== 1)
      $fatal(1, "unique_if_taken was %0d, expected 1", unique_if_taken);
    if (priority_case_taken !== 1)
      $fatal(
          1, "priority_case_taken was %0d, expected 1", priority_case_taken);
    if (unique_case_taken !== 1)
      $fatal(1, "unique_case_taken was %0d, expected 1", unique_case_taken);
    $display("All checks passed");
  end
endmodule

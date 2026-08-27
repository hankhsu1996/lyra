// A wait statement evaluates its condition, and if the condition is not true it
// blocks the statements that follow until the condition becomes true; if the
// condition already holds when the statement is reached, nothing is delayed and
// the statement it guards runs in the same time step (LRM 9.4.3, 12.4). Not
// true covers a zero value and an unknown one alike, so a condition that is x
// blocks exactly as a condition that is 0 does. The whole condition is
// re-evaluated when anything it reads changes, so a condition made of two
// terms releases the procedure only once both of them hold, and the guarded
// statement may be omitted entirely.
module Top;
  bit already;
  bit released;
  bit first_term, second_term;
  logic unknown_scalar;
  logic [7:0] unknown_operand;

  int already_marker;
  time already_time;
  int released_marker;
  time released_time;
  time both_terms_time;
  time empty_body_time;
  time unknown_scalar_time;
  time unknown_operand_time;

  initial begin
    already = 1'b1;
    wait (already) already_marker = 1;
    already_time = $time;
  end

  initial begin
    wait (released) released_marker = 1;
    released_time = $time;
  end

  initial begin
    wait (first_term && second_term);
    both_terms_time = $time;
  end

  initial begin
    wait (released);
    empty_body_time = $time;
  end

  initial begin
    wait (unknown_scalar);
    unknown_scalar_time = $time;
  end

  initial begin
    wait (unknown_operand > 8'hF0);
    unknown_operand_time = $time;
  end

  initial begin
    #5;
    released = 1'b1;
    first_term = 1'b1;
    unknown_scalar = 1'b1;
    unknown_operand = 8'hFF;
    #5;
    second_term = 1'b1;
  end

  final begin
    if (already_marker !== 1)
      $fatal(1, "already_marker was %0d, expected 1", already_marker);
    if (already_time !== 0)
      $fatal(1, "already_time was %0d, expected 0", already_time);
    if (released_marker !== 1)
      $fatal(1, "released_marker was %0d, expected 1", released_marker);
    if (released_time !== 5)
      $fatal(1, "released_time was %0d, expected 5", released_time);
    if (both_terms_time !== 10)
      $fatal(1, "both_terms_time was %0d, expected 10", both_terms_time);
    if (empty_body_time !== 5)
      $fatal(1, "empty_body_time was %0d, expected 5", empty_body_time);
    if (unknown_scalar_time !== 5)
      $fatal(1, "unknown_scalar_time was %0d, expected 5",
             unknown_scalar_time);
    if (unknown_operand_time !== 5)
      $fatal(1, "unknown_operand_time was %0d, expected 5",
             unknown_operand_time);
    $display("All checks passed");
  end
endmodule

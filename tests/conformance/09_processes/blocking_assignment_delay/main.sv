// A blocking assignment carrying an intra-assignment delay reads its right-hand
// side where the statement is reached, then suspends the procedure until the
// delay elapses and only then makes the assignment (LRM 9.4.5, Table 9-3). That
// order is what makes the data swap of LRM 9.4.5 work: both right-hand sides
// are read before either delay elapses.
module Top;
  int source = 1;
  int target = 9;
  int completed_at = 9;
  int swapped_a = 9;
  int swapped_b = 9;

  initial begin
    target       = #10 source;
    completed_at = $time;
  end

  // Rewriting the operand during the delay reaches neither assignment.
  initial #5 source = 99;

  initial begin
    swapped_a = 3;
    swapped_b = 4;
    fork
      swapped_a = #5 swapped_b;
      swapped_b = #5 swapped_a;
    join
  end

  final begin
    if (target !== 1)
      $fatal(1, "the assignment stored %0d, expected the operand read at time zero", target);
    if (completed_at !== 10)
      $fatal(1, "the assignment completed at time %0d, expected 10", completed_at);
    if (swapped_a !== 4 || swapped_b !== 3)
      $fatal(1, "the swap left a=%0d b=%0d, expected a=4 b=3", swapped_a, swapped_b);
    $display("All checks passed");
  end
endmodule

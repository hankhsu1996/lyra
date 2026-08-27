// Delay values for each statement of a parallel block are relative to the
// simulation time of entering the block rather than to the statement before
// them, and control passes out of the block when the last time-ordered
// statement executes (LRM 9.3.2). The order the statements are written in
// therefore changes neither when each one runs nor when the block finishes
// (LRM 9.3.3). The same two delays in a sequential block, where each is
// relative to the execution time of the previous statement, accumulate instead
// (LRM 9.3.1).
module Top;
  int forward_a, forward_b, forward_end;
  int reverse_a, reverse_b, reverse_end;
  int step_one, step_two, sequential_end;

  initial begin
    fork
      #10 forward_a = $time;
      #20 forward_b = $time;
    join
    forward_end = $time;

    fork
      #20 reverse_b = $time;
      #10 reverse_a = $time;
    join
    reverse_end = $time;

    begin
      #10 step_one = $time;
      #20 step_two = $time;
    end
    sequential_end = $time;
  end

  final begin
    if (forward_a !== 10)
      $fatal(1, "forward_a was %0d, expected 10", forward_a);
    if (forward_b !== 20)
      $fatal(1, "forward_b was %0d, expected 20", forward_b);
    if (forward_end !== 20)
      $fatal(1, "forward_end was %0d, expected 20", forward_end);
    if (reverse_a !== 30)
      $fatal(1, "reverse_a was %0d, expected 30", reverse_a);
    if (reverse_b !== 40)
      $fatal(1, "reverse_b was %0d, expected 40", reverse_b);
    if (reverse_end !== 40)
      $fatal(1, "reverse_end was %0d, expected 40", reverse_end);
    if (step_one !== 50)
      $fatal(1, "step_one was %0d, expected 50", step_one);
    if (step_two !== 70)
      $fatal(1, "step_two was %0d, expected 70", step_two);
    if (sequential_end !== 70)
      $fatal(1, "sequential_end was %0d, expected 70", sequential_end);
    $display("All checks passed");
  end
endmodule

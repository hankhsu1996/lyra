// $finish causes the simulator to exit and pass control back to the host
// operating system; its optional argument selects which diagnostic message
// the tool prints and nothing else (LRM 20.2, Table 20-1).
// Simulation ends where the call is reached: the statements after it do not
// run, the loop around it is not resumed, and a process waiting for a later
// time never resumes at all. A final procedure runs when simulation ends due
// to an explicit call to $finish (LRM 9.2.3).
module Top;
  int iterations;
  int iteration_completed;
  int after_loop;
  int pending_write;

  initial begin
    iteration_completed = 9;
    after_loop = 7;
    pending_write = 7;
    #5;
    while (iterations < 100) begin
      iterations = iterations + 1;
      if (iterations >= 3) $finish(0);
      iteration_completed = iterations;
    end
    after_loop = 1;
  end

  initial begin
    #10;
    pending_write = 1;
  end

  final begin
    if (iterations !== 3)
      $fatal(1, "the loop body ran %0d times, expected 3", iterations);
    if (iteration_completed !== 2)
      $fatal(1, "iteration_completed was %0d, expected 2",
             iteration_completed);
    if (after_loop !== 7)
      $fatal(1, "after_loop was %0d, expected 7", after_loop);
    if (pending_write !== 7)
      $fatal(1, "pending_write was %0d, expected 7", pending_write);
    $display("All checks passed");
  end
endmodule

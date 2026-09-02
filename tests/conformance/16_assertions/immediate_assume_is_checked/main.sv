// @reports: assumption failed
//
// An immediate assume states what the environment is assumed to do rather than
// what the design is obliged to do, and a simulation tool checks it exactly as
// it checks an immediate assert: the same action block selection, and the same
// default report when a false expression supplies no fail statement
// (LRM 16.3).
module Top;
  int fail_hits;
  int completed;

  initial begin
    fail_hits = 0;
    completed = 0;

    assume (1) else fail_hits = fail_hits + 1;

    assume (0) else fail_hits = fail_hits + 1;

    assume (0);

    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed assumption stopped the procedure that reached it");
    if (fail_hits !== 1) $fatal(1, "fail_hits was %0d, expected 1", fail_hits);
    $display("All checks passed");
  end
endmodule

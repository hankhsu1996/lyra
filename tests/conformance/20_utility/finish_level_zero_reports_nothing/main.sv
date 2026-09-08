// @reports-nothing:
//
// The optional argument of $finish selects the diagnostic message the tool
// prints, and level 0 prints nothing (LRM 20.2, Table 20-1). Simulation still
// ends where the call is reached and a final procedure still runs, because
// what the argument selects is the message and nothing else.
module Top;
  int reached_after_finish;
  int completed;

  initial begin
    completed = 3;
    reached_after_finish = 7;
    completed = 1;
    $finish(0);
    reached_after_finish = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "completed was %0d, expected 1", completed);
    if (reached_after_finish !== 7)
      $fatal(1, "reached_after_finish was %0d, expected 7",
             reached_after_finish);
    $display("All checks passed");
  end
endmodule

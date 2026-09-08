// @reports: $finish
//
// $finish called with no argument takes a level of 1, which prints the
// simulation time and the location of the call (LRM 20.2, Table 20-1). A
// program cannot read a message about itself, so what the report says is
// claimed above and everything else is checked here.
//
// This is the other direction of the same requirement: level 0 prints nothing,
// and the default prints something, so an implementation that is uniformly
// silent or uniformly noisy fails one of the two.
module Top;
  int reached_after_finish;
  int completed;

  initial begin
    completed = 3;
    reached_after_finish = 7;
    #5;
    completed = 1;
    $finish;
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

// A task is enabled by a statement within a procedural block rather than as
// an operand of an expression, because a task returns no value. The empty
// parentheses of the enable may be dropped when the task takes no arguments,
// and it is legal for a task to have no statements at all, in which case
// enabling it does nothing (LRM 13.2, 13.3, 13.5, 13.5.5).
module Top;
  int counter;
  int after_noop;

  task automatic noop;
  endtask

  task automatic tick();
    counter = counter + 1;
  endtask

  initial begin
    counter = 5;
    noop;
    after_noop = counter;
    tick();
    tick;
    tick();
  end

  final begin
    if (after_noop !== 5)
      $fatal(1, "after_noop was %0d, expected 5", after_noop);
    if (counter !== 8) $fatal(1, "counter was %0d, expected 8", counter);
    $display("All checks passed");
  end
endmodule

// A task exits when endtask is reached, and a return statement exits it
// before then. A task returns no value, so its return carries no expression;
// what a return does is end the task, which means the copy back of the output
// and inout formals happens on that path just as it does at endtask
// (LRM 13.2, 13.3).
module Top;
  int clamped;
  int unclamped;
  int tail_runs;

  task automatic clamp(inout int v);
    if (v > 10) begin
      v = 10;
      return;
    end
    v = v + 100;
    tail_runs = tail_runs + 1;
  endtask

  initial begin
    tail_runs = 0;
    clamped = 50;
    clamp(clamped);
    unclamped = 3;
    clamp(unclamped);
  end

  final begin
    if (clamped !== 10) $fatal(1, "clamped was %0d, expected 10", clamped);
    if (unclamped !== 103)
      $fatal(1, "unclamped was %0d, expected 103", unclamped);
    if (tail_runs !== 1)
      $fatal(1, "tail_runs was %0d, expected 1", tail_runs);
    $display("All checks passed");
  end
endmodule

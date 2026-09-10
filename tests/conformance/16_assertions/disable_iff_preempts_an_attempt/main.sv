// A `disable iff` clause preempts an evaluation attempt. If its condition is
// true at any time between the start of the attempt and the end of that
// attempt, both inclusive, the evaluation is disabled -- which is neither a
// success nor a failure, so no statement of the action block runs (LRM 16.12,
// 16.14.1).
//
// The condition is read on the current values of its variables rather than on
// sampled ones (LRM 16.12), and "at any time" is the whole of the interval and
// not only its ticks. Both readings are exercised: the first reset is high
// across a tick, while the second is a pulse that begins and ends between two
// ticks and is therefore low at every tick an attempt reads.
module Top;
  logic clk = 0;
  logic rst = 0;
  logic a = 0;
  logic b = 0;

  int passes = 0;
  int fails = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35, 45, 55 and 65. `a` is sampled high at the
  // ticks at 15 and at 45, and `b` is low throughout, so each of those two
  // attempts would fail a tick later were it not preempted.
  initial begin
    #8;
    a = 1;
    #10;
    a = 0;
    #2;
    rst = 1;
    #10;
    rst = 0;
    #8;
    a = 1;
    #10;
    a = 0;
    rst = 1;
    #4;
    rst = 0;
    #16 $finish;
  end

  a_reset_preempts: assert property (@(posedge clk) disable iff (rst) a |=> b)
      passes = passes + 1;
    else
      fails = fails + 1;

  final begin
    // Three attempts are disabled: the two whose antecedent matched, and the
    // one that began at the tick at 25 while the first reset was still high.
    // The remaining four succeed vacuously.
    if (fails !== 0)
      $fatal(1, "%0d attempts failed, expected none to survive the reset",
             fails);
    if (passes !== 4)
      $fatal(1, "%0d attempts held, expected 4", passes);
    $display("All checks passed");
  end
endmodule

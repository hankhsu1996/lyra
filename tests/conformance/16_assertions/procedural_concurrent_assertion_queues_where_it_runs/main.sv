// A concurrent assertion embedded in a procedure is not evaluated where it is
// reached. Reaching it places a pending instance on a queue belonging to the
// running process; the instance matures in the Observed region of that time
// step, and an evaluation attempt begins there because the step carries a tick
// of the assertion's leading clock (LRM 16.14.6). So the attempts are the
// executions rather than the ticks.
//
// The clock is not written on the assertion, and is inferred from the procedure
// around it: the block has no blocking timing control, exactly one event
// control, and `clk` appears nowhere else in the body (LRM 16.14.6).
//
// The conditional that decides whether the assertion is reached reads the
// current value of `en`, not its sampled value -- which is the opposite of the
// rule for the assertion's own expressions (LRM 16.14.6, 16.5.1). `en` is
// toggled immediately above the conditional, so the two readings select
// complementary ticks and the count tells them apart.
module Top;
  logic clk = 0;
  logic ok = 1;
  logic en = 0;

  int runs = 0;
  int hits = 0;

  always #5 clk = ~clk;

  always @(posedge clk) begin
    runs = runs + 1;
    en = ~en;
    if (en) assert property (ok) hits = hits + 1;
  end

  initial #48 $finish;

  final begin
    // Ticks land at 5, 15, 25, 35 and 45. `en` is high after the toggle at the
    // first, third and fifth of them.
    if (runs !== 5)
      $fatal(1, "the procedure ran %0d times, expected 5", runs);
    if (hits !== 3)
      $fatal(1, "%0d attempts held, expected 3", hits);
    $display("All checks passed");
  end
endmodule

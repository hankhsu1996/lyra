// The sampled value of a clocking block input is the value its block samples
// at the clocking event (LRM 16.5.1, 14.13), so a concurrent assertion clocked
// by that event reads `cb.b` as it reads `b` (LRM 16.18), and the sampled value
// functions read it as of each tick the same way. An input skewed by a delay
// samples what its signal held that long before the event.
module Top;
  bit clk;
  int b = 0;
  int d = 0;
  int fails = 0;
  int passes = 0;
  int checks = 0;
  int failures = 0;

  clocking cb @(posedge clk);
    input b;
    input #2 d;
  endclocking

  always #5 clk = !clk;
  // `b` changes between two ticks; `d` changes 1 before each tick, after the
  // point its skew samples it at.
  always @(negedge clk) b = b + 1;
  initial forever begin
    #4 d = d + 10;
    #6;
  end

  a_b: assert property (@(posedge clk) cb.b == b) passes++;
    else fails++;

  int d_seen[$];
  always @(posedge clk) begin
    checks++;
    if ($sampled(cb.b) !== $sampled(b)) begin
      failures++;
      $display("$sampled(cb.b) was %0d, expected %0d", $sampled(cb.b),
               $sampled(b));
    end
    checks++;
    if ($past(cb.b) !== $past(b)) begin
      failures++;
      $display("$past(cb.b) was %0d, expected %0d", $past(cb.b), $past(b));
    end
    // At each tick `d` was raised 1 earlier, so 2 before the tick it held the
    // value before that.
    checks++;
    if ($sampled(cb.d) !== d - 10) begin
      failures++;
      $display("$sampled(cb.d) was %0d, expected %0d", $sampled(cb.d), d - 10);
    end
  end

  initial #50 $finish;

  final begin
    if (fails != 0) $fatal(1, "a_b failed %0d times", fails);
    if (passes != 5) $fatal(1, "a_b passed %0d times, expected 5", passes);
    if (checks != 15) $fatal(1, "%0d checks ran, expected 15", checks);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

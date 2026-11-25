// LRM 14.4 samples an input "with explicit #0 skew" in the Observed region,
// and LRM 14.13 every other at the end of the time step its skew names before
// the event. A skew below the scope's precision rounds to no step (LRM
// 3.14.1), so it names the event's own time step, whose end comes after the
// event, and neither sentence answers it. Lyra samples it as `#0`: in the
// Observed region, after the event's nonblocking updates, so a process its
// sample's change wakes runs after the block's event and is reached by the
// next one.
module Top;
  timeunit 1ns;
  timeprecision 100ps;
  logic clk = 0;
  always #10 clk = ~clk;

  int d = 0;
  logic w = 0;
  clocking cb @(posedge clk);
    input #0.01 x = d;
    input #0.01 y = w;
  endclocking

  always @(posedge clk) if ($time == 30) d <= 3;
  initial #12 w = 1;

  int seen = -1;
  time woken = 0;
  bit done = 0;
  initial begin
    @(cb);       // 10
    @(cb);       // 30
    seen = cb.x;
    @(cb);       // 50
    done = 1;
    #1 $finish;
  end
  initial begin
    @(posedge cb.y);
    @(cb);
    woken = $time;
  end

  final begin
    if (!done) $fatal(1, "the block's events were not all reached");
    if (seen !== 3) $fatal(1, "the input at 30 was %0d, expected 3", seen);
    if (woken !== 50) $fatal(1, "woken by its sample, the block's event came at %0t, expected 50", woken);
    $display("All checks passed");
  end
endmodule

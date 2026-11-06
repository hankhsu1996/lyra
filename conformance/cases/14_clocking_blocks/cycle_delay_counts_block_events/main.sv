// A cycle delay waits for that many events of the default clocking (LRM
// 14.11, 14.12): made between events, `##1` waits only the rest of the cycle.
// `##0` goes on at once where the block's event has occurred in the current
// time step and otherwise waits for its next occurrence, and the count may be
// any expression, evaluated where the delay is reached. A process waiting on
// the block resumes after the Observed region the block's event is triggered
// in (LRM 14.10), so it sees the nonblocking updates of the event's time step,
// and `##0` reached at the clock's edge, before that region, waits for it.
module Top;
  logic clk = 0;
  always #10 clk = ~clk;  // rises at 10, 30, 50, ..., 150

  default clocking cb @(posedge clk);
  endclocking

  time after_fraction = 0, after_zero_here = 0, after_two = 0;
  time after_zero_later = 0, after_counted = 0;
  int n = 2;
  bit done = 0;

  // How many rising edges have gone by, updated in the NBA region.
  int edges = 0;
  always @(posedge clk) edges <= edges + 1;
  int edges_after_fraction = -1, edges_at_edge = -1;
  time after_zero_at_edge = 0;

  initial begin
    #5;
    ##1;
    after_fraction = $time;      // 10
    edges_after_fraction = edges;
    ##0;
    after_zero_here = $time;     // still 10
    ##2;
    after_two = $time;           // 50
    #1;
    ##0;
    after_zero_later = $time;    // 70
    ##(n + 1);
    after_counted = $time;       // 130
    @(posedge clk);              // 150, before the block's event
    ##0;
    after_zero_at_edge = $time;  // still 150
    edges_at_edge = edges;
    done = 1;
    $finish;
  end

  final begin
    if (!done) $fatal(1, "the delays did not all end");
    if (after_fraction !== 10) $fatal(1, "##1 from 5 ended at %0t, expected 10", after_fraction);
    if (after_zero_here !== 10) $fatal(1, "##0 at an event ended at %0t, expected 10", after_zero_here);
    if (after_two !== 50) $fatal(1, "##2 ended at %0t, expected 50", after_two);
    if (after_zero_later !== 70) $fatal(1, "##0 between events ended at %0t, expected 70", after_zero_later);
    if (after_counted !== 130) $fatal(1, "##(n + 1) ended at %0t, expected 130", after_counted);
    if (edges_after_fraction !== 1) $fatal(1, "##1 from 5 saw %0d edges, expected 1", edges_after_fraction);
    if (after_zero_at_edge !== 150) $fatal(1, "##0 at the clock's edge ended at %0t, expected 150", after_zero_at_edge);
    if (edges_at_edge !== 8) $fatal(1, "##0 at the clock's edge saw %0d edges, expected 8", edges_at_edge);
    $display("All checks passed");
  end
endmodule

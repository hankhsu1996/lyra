// A nonblocking assignment evaluates its right-hand side when the statement
// executes but updates its left-hand side only at the end of the time step, so
// several of them in one procedure all read the values current at the edge and
// none of them sees another's new value (LRM 10.4.2). A chain of stages written
// this way therefore advances by exactly one stage per edge whichever order the
// stages appear in, and two stages that read each other each take the other's
// value from before the edge rather than what it was just given.
module Top;
  bit clk;
  int d;
  int q1;
  int q2;
  int r2;
  int r1;
  int x;
  int y;

  int q1_after_one_edge;
  int q2_after_one_edge;

  always_ff @(posedge clk) begin
    q1 <= d;
    q2 <= q1;
  end

  always_ff @(posedge clk) begin
    r2 <= r1;
    r1 <= d;
  end

  always_ff @(posedge clk) begin
    x <= y + 1;
    y <= x;
  end

  initial begin
    d = 10;
    #5 clk = 1;
    #2 q1_after_one_edge = q1;
    q2_after_one_edge = q2;
    #1 d = 20;
    #2 clk = 0;
    #5 clk = 1;
    #3 d = 30;
    #2 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
    #1;
  end

  final begin
    if (q1_after_one_edge !== 10)
      $fatal(1, "q1_after_one_edge was %0d, expected 10", q1_after_one_edge);
    if (q2_after_one_edge !== 0)
      $fatal(1, "q2_after_one_edge was %0d, expected 0", q2_after_one_edge);
    if (q1 !== 30) $fatal(1, "q1 was %0d, expected 30", q1);
    if (q2 !== 20) $fatal(1, "q2 was %0d, expected 20", q2);
    if (r1 !== 30) $fatal(1, "r1 was %0d, expected 30", r1);
    if (r2 !== 20) $fatal(1, "r2 was %0d, expected 20", r2);
    if (x !== 2) $fatal(1, "x was %0d, expected 2", x);
    if (y !== 1) $fatal(1, "y was %0d, expected 1", y);
    $display("All checks passed");
  end
endmodule

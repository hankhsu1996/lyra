// `$past`'s third argument gates the clocking event rather than the expression:
// the sampling of the first argument is performed on its clock gated with that
// expression (LRM 16.9.3), so the event counted is `posedge clk iff en` and a
// tick where the gate does not hold is not a prior tick at all.
//
// Two reads of one variable in one procedure therefore reach different time
// steps -- the ungated read counts every tick, the gated read counts only the
// ticks the gate admitted -- which is what makes the gate part of the event and
// not a condition on the answer.
module Top;
  logic clk = 0;
  logic en = 1;
  int v = 7;

  int ticks = 0;
  int ungated;
  int gated1;
  int gated2;

  always #5 clk = ~clk;

  always @(posedge clk) v = v + 10;

  always @(posedge clk) begin
    ticks = ticks + 1;
    if (ticks == 5) begin
      ungated = $past(v);
      gated1 = $past(v, 1, en);
      gated2 = $past(v, 2, en);
    end
  end

  initial begin
    v = 3;
    // Leaves the gate open across the ticks at 5 and 15 and shut from then on.
    #18 en = 0;
    #34 $finish;
  end

  final begin
    if (ticks < 5) $fatal(1, "the clock reached %0d ticks, expected 5", ticks);
    // Ticks land at 5, 15, 25, 35 and 45, where the sampled value of `v` is 3,
    // 13, 23, 33 and 43. The gate admitted only the first two.
    if (ungated !== 33)
      $fatal(1, "the ungated read was %0d, expected 33", ungated);
    if (gated1 !== 13)
      $fatal(1, "the first prior gated tick was %0d, expected 13", gated1);
    if (gated2 !== 3)
      $fatal(1, "the second prior gated tick was %0d, expected 3", gated2);
    $display("All checks passed");
  end
endmodule

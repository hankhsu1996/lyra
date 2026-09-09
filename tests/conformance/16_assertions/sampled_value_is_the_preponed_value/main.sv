// A sampled value is the value a variable held in the Preponed region of the
// time step (LRM 16.5.1), which is its value before anything in that step ran.
//
// Two procedures wake on the same clock edge, and the Active region may run
// them in either order (LRM 4.4.2.2), so a plain read of `d` from the second
// one is a race. The sampled value is not: the Preponed region precedes both,
// so `$sampled(d)` answers the same whichever order they took. At the first
// edge `d` has not been written yet, so the sampled value is its initial 0; at
// the second it was written a step earlier, so the sampled value is 1.
//
// `$sampled` is the one sampled value function that uses no clocking event
// (LRM 16.9.3), so this states the retained value alone.
module Top;
  logic clk = 0;
  logic d = 0;
  int edges = 0;
  int sampled_at_first;
  int sampled_at_second;

  always #5 clk = ~clk;

  always @(posedge clk) d = 1;

  always @(posedge clk) begin
    edges = edges + 1;
    if (edges == 1) sampled_at_first = $sampled(d);
    if (edges == 2) sampled_at_second = $sampled(d);
  end

  initial begin
    #20;
    $finish;
  end

  final begin
    if (edges < 2) $fatal(1, "the clock did not reach two positive edges");
    if (sampled_at_first !== 0)
      $fatal(
          1, "the sampled value at the first edge was %0d, expected 0",
          sampled_at_first);
    if (sampled_at_second !== 1)
      $fatal(
          1, "the sampled value at the second edge was %0d, expected 1",
          sampled_at_second);
    $display("All checks passed");
  end
endmodule

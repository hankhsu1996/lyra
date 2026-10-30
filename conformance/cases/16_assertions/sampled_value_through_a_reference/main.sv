// A `ref` port and the variable it connects to are one piece of storage (LRM
// 23.3.3.2), so a sampled value read through the port is that variable's: the
// value it held in the Preponed region of the time step (LRM 16.5.1), not
// whatever the port's own side of the connection last saw.
//
// The parent writes `d` on the same edge the child samples it, so a plain read
// would be a race; the sampled value is the pre-step 0 whichever order the two
// procedures took. Only the first edge is captured, because by the second `d`
// was already 1 before the step began.
module Leaf (
    ref logic r,
    input logic clk
);
  int edges = 0;
  int seen;

  always @(posedge clk) begin
    edges = edges + 1;
    if (edges == 1) seen = $sampled(r);
  end
endmodule

module Top;
  logic clk = 0;
  logic d = 0;

  Leaf leaf (
      .r  (d),
      .clk(clk)
  );

  always #5 clk = ~clk;

  always @(posedge clk) d = 1;

  initial begin
    #20;
    $finish;
  end

  final begin
    if (leaf.edges < 1) $fatal(1, "the clock did not reach a positive edge");
    if (leaf.seen !== 0)
      $fatal(
          1, "the sampled value read through the reference was %0d, expected 0",
          leaf.seen);
    $display("All checks passed");
  end
endmodule

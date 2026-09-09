// A `ref` port and the variable it connects to are one piece of storage (LRM
// 23.3.3.2), so a process may wait on the port's own name and it is the
// connected variable's changes it waits for.
//
// The port's internal name owns no cell of its own -- it stands for the
// connected variable's -- and a wait registers on a cell, so every form of
// waiting is checked here: an edge event control (LRM 9.4.2), the implicit
// sensitivity an `always_comb` and an `always @*` infer (LRM 9.2.2.2.1,
// 9.4.2.2), and a level-sensitive `wait` (LRM 9.4.3).
module Leaf (ref logic r);
  int edges = 0;
  int waited = 0;
  logic inverted;
  logic followed;

  always @(posedge r) edges = edges + 1;

  always_comb inverted = ~r;

  always @* followed = r;

  initial begin
    wait (r == 1'b1);
    waited = 1;
  end
endmodule

module Top;
  logic clk = 0;

  Leaf leaf (.r(clk));

  always #5 clk = ~clk;

  initial begin
    #22;
    $finish;
  end

  final begin
    if (leaf.edges !== 2)
      $fatal(
          1, "the child saw %0d positive edges through the ref port, expected 2",
          leaf.edges);
    if (leaf.waited !== 1)
      $fatal(1, "the level-sensitive wait on the ref port never fired");
    if (leaf.inverted !== ~clk)
      $fatal(1, "the child's combinational read did not follow the ref port");
    if (leaf.followed !== clk)
      $fatal(1, "the child's implicit-sensitivity read did not follow the port");
    $display("All checks passed");
  end
endmodule

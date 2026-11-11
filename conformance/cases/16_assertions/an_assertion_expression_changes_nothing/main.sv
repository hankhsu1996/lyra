// @rejected:
//
// Evaluating an expression that appears in a concurrent assertion shall have no
// side effects, so a conforming tool rejects this program, whose assertion pops
// an element from the queue it reads at every tick (LRM 16.6).
module Top;
  logic clk = 0;
  int q[$] = '{1, 2, 3, 4};

  always #5 clk = ~clk;

  a_pop: assert property (@(posedge clk) q.pop_front() > 0);

  initial #22 $finish;

  final $display("the program the standard rejects ran with %0d", q.size());
endmodule

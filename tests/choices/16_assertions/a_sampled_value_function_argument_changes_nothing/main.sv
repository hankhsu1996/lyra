// @rejected:
//
// A sampled value function may be called in procedural code (LRM 16.9.3), and
// LRM 16.6, which forbids an expression of a concurrent assertion any side
// effect, does not say whether its argument there is such an expression. The
// sampled value of an expression is computed from the sampled values of its
// operands (LRM 16.5.1), so a variable the argument names stands for what it
// held at the tick read, which is fixed, and a write to it has no meaning
// the standard gives. Lyra takes the stricter
// reading, in the front end it pins, which already holds this argument to LRM
// 16.6's rule on output and ref arguments, and refuses it as an argument of an
// assertion.
module Top;
  logic clk = 0;
  int x = 0;
  int r;

  always #5 clk = ~clk;

  always @(posedge clk) r <= $past(x++);

  initial #22 $finish;

  final $display("the program the standard rejects ran with %0d", x);
endmodule

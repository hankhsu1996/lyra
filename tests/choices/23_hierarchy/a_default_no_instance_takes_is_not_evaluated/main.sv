// A parameter an instantiation assigns takes the value it is given, and one it
// leaves unassigned takes the default its declaration states (LRM 6.20.1,
// 23.10). Whether a default every instance overrides has to be a valid
// constant expression the standard does not say. Lyra evaluates a default only
// for an instance left to it, so one that could not be evaluated, here a call
// of a parameter as though it were a function, stops nothing where every
// instance assigns the parameter. This case holds Lyra to that.
module Leaf #(
    parameter int W = 8,
    parameter int NEVER = W(1)
) (
    output int w
);
  initial w = W + NEVER;
endmodule

module Top;
  int w[2];

  Leaf #(.W(4), .NEVER(0)) first (w[0]);
  Leaf #(.NEVER(3)) second (w[1]);

  final begin
    if (w[0] !== 4 || w[1] !== 11)
      $fatal(1, "the instances held %0d and %0d, expected 4 and 11", w[0],
             w[1]);
    $display("All checks passed");
  end
endmodule

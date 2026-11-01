// One module instantiated once per block of a generate loop, each instance
// reading a variable of the module instantiating it by a hierarchical name
// (LRM 23.6). N is how many instances there are.
//
// The front end shares no elaborated body a name leaves upward, so each
// instance is lowered to HIR on its own and found to come out as the one body.
// @each-adds: lowerings 1
module Leaf (input var bit c, output int o);
  always @(posedge c) o <= Top.seed;
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int seed;
  int o [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    Leaf u (.c(clk), .o(o[i]));
  end
endmodule

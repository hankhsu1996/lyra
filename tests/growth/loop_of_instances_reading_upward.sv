// One module instantiated once per block of a generate loop, each instance
// reading a variable of the module instantiating it by a hierarchical name
// (LRM 23.6). N is how many instances there are.
module Leaf (input bit c, output int o);
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

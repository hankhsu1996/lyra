// One module instantiated once per block of a generate loop, each instance
// handed a different value it only reads (LRM 23.10). N is how many instances
// there are.
module Leaf #(parameter int K = 0) (input bit c, output int o);
  always @(posedge c) o <= K;
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int o [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    Leaf #(.K(i)) u (.c(clk), .o(o[i]));
  end
endmodule

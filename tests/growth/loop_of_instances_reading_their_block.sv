// One module instantiated once per block of a generate loop, each instance
// reading a variable of the block holding it by the block's own name (LRM
// 23.8). N is how many blocks there are.
module Leaf (input bit c, output int o);
  always @(posedge c) o <= cell_block.seed;
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int o [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    if (1) begin : cell_block
      int seed;
      Leaf u (.c(clk), .o(o[i]));
    end
  end
endmodule

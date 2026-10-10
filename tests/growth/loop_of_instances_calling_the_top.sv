// One module instantiated once per block of a generate loop, each instance
// calling a function of the top-level module by a hierarchical name (LRM
// 23.6). N is how many instances there are.
module Leaf (input bit c, input int d, output int q);
  always @(posedge c) q <= Top.bump(d);
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int o [N];
  function automatic int bump(int v);
    return v + 1;
  endfunction
  for (genvar i = 0; i < N; i += 1) begin : g
    Leaf u (.c(clk), .d(7), .q(o[i]));
  end
endmodule

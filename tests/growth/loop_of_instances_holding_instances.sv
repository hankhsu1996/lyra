// One module instantiated once per block of a generate loop, every instance
// alike and each holding instances of another module (LRM 23.3). N is how many
// of the outer instances there are.
module Leaf (input bit c, input int d, output int q);
  always @(posedge c) q <= d + 1;
endmodule

module Mid (input bit c, input int d, output int q);
  int t;
  Leaf first (.c(c), .d(d), .q(t));
  Leaf second (.c(c), .d(t), .q(q));
endmodule

module Top #(parameter int N = 4);
  bit clk;
  int o [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    Mid m (.c(clk), .d(7), .q(o[i]));
  end
endmodule

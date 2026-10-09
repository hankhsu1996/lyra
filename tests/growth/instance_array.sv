// One module instantiated as an array of instances (LRM 23.3.2). N is how many
// elements the array has.
module Leaf (input bit c);
  int count;
  always @(posedge c) count <= count + 1;
endmodule

module Top #(parameter int N = 4);
  bit clk;
  Leaf u [N] (.c(clk));
endmodule

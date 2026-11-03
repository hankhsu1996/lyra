// N event controls in the one loop of a process: each wait stands for the whole
// loop, and what they end on the way out is built once however many stops
// reach it.
// @each-adds: bytes,
// @each-adds: mir.captures 2, mir.closures 1, mir.constants 1,
// @each-adds: mir.types 3,
// @each-adds: lir.captures 2, lir.closures 1, lir.functions 2,
// @each-adds: lir.constants 1, lir.type steps 1, lir.types 4
module Top #(parameter int N = 4);
  bit clk;
  int c;

  always begin
    [[@(posedge clk) c = {i};| ]]
  end
endmodule

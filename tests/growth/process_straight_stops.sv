// N event controls one after another in one process: each is built, stopped at
// and ended on its own, so a step adds its own code and nothing to the others'.
// @each-adds: bytes,
// @each-adds: mir.captures 2, mir.closures 1, mir.constants 1,
// @each-adds: mir.types 3,
// @each-adds: lir.captures 2, lir.closures 1, lir.functions 2,
// @each-adds: lir.constants 1, lir.type steps 1, lir.types 4
module Top #(parameter int N = 4);
  bit clk;
  int c;

  initial begin
    [[@(posedge clk) c = {i};| ]]
  end
endmodule

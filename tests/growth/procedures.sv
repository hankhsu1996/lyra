// N clocked procedures, each a `begin`-`end` around one statement on a
// variable of its own. A procedure is no branch of the hierarchy (LRM 23.6) and
// an unnamed block without declarations no scope at all (LRM 9.3.4), so a step
// adds the procedure's own body and its variable, and no class.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.callables 2, lir.functions 6, lir.type steps 2,
// @each-adds: mir.closures 2, lir.closures 2, mir.captures 4, lir.captures 4,
// @each-adds: mir.class constants 1, lir.constants 1,
// @each-adds: mir.types 6, lir.types 7
module Top #(parameter int N = 4);
  logic clk;
  logic [7:0] [[q{i}|, ]];

[[  always @(posedge clk) begin q{i} <= q{i} + 1; end|
]]
endmodule

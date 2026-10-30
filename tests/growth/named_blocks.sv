// N clocked procedures, each a named block declaring a static of its own. A
// named block is a scope a path and a `disable` reach (LRM 9.3.4, 9.6.2), and
// what it keeps -- its static and what a `disable` of it ends -- is one cell
// each per instance (LRM 6.21), so a step adds those cells and the procedure's
// own body, and no class.
// @each-adds: bytes,
// @each-adds: mir.fields 3, lir.members 3,
// @each-adds: mir.callables 2, lir.functions 6, lir.type steps 2,
// @each-adds: mir.closures 2, lir.closures 2, mir.captures 4, lir.captures 4,
// @each-adds: mir.class constants 3, lir.constants 3,
// @each-adds: mir.types 6, lir.types 7
module Top #(parameter int N = 4);
  logic clk;
  logic [7:0] [[q{i}|, ]];

[[  always @(posedge clk) begin : b{i}
    static logic [7:0] s = 0;
    s = s + q{i};
    q{i} <= s;
  end|
]]
endmodule

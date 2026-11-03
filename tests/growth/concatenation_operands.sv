// A concatenation of N one-bit operands, each written at a position of its own.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.constants 2, lir.constants 2
module Top #(parameter int N = 4);
  bit [N-1:0] v;
  bit [[b{i}|, ]];

  initial v = {[[b{i}|, ]]};
endmodule

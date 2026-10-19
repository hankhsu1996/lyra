// A concatenation of N one-bit operands.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1
module Top #(parameter int N = 4);
  bit [N-1:0] v;
  bit [[b{i}|, ]];

  initial v = {[[b{i}|, ]]};
endmodule

// A child of N output ports, each connected to its own variable.
// @each-adds: bytes,
// @each-adds: mir.callables 4, lir.functions 4, mir.fields 3, lir.members 3,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Child #(parameter int N = 4) ([[output int p{i}|, ]]);
  [[assign p{i} = {i};| ]]
endmodule

module Top #(parameter int N = 4);
  int [[w{i}|, ]];

  Child #(.N(N)) c([[.p{i}(w{i})|, ]]);
endmodule

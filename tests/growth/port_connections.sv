// A child of N output ports, each connected to its own variable. Each port's
// assignment has a record of its kind and where it is written, and it and the
// port's drive a record of how its body is called; the drives are written at
// the one instance and share theirs.
// @each-adds: bytes,
// @each-adds: mir.callables 4, lir.functions 4, mir.fields 3, lir.members 3,
// @each-adds: mir.constants 8, lir.constants 8
module Child #(parameter int N = 4) ([[output int p{i}|, ]]);
  [[assign p{i} = {i};| ]]
endmodule

module Top #(parameter int N = 4);
  int [[w{i}|, ]];

  Child #(.N(N)) c([[.p{i}(w{i})|, ]]);
endmodule

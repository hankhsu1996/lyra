// Operations over N operands the source lists side by side: a join of
// strings, a join of queue elements, a replication and a stream of a list,
// and a set of N members, each a list one step makes one longer and no deeper
// (LRM 11.4.12, 10.10, 11.4.12.1, 11.4.14, 11.4.13).
// @each-adds: bytes,
// @each-adds: mir.fields 2, lir.members 2,
// @each-adds: mir.class constants 1, lir.constants 1,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  int [[v{i}|, ]];
  string [[s{i}|, ]];
  string text;
  int q[$];
  bit [64*N-1:0] replicated;
  bit [32*N-1:0] streamed;
  bit found;

  initial begin
    text = {[[s{i}|, ]]};
    q = {q, [[v{i}|, ]]};
    replicated = {2{[[v{i}|, ]]}};
    streamed = {<<{[[v{i}|, ]]}};
    found = v0 inside {[[v{i}|, ]]};
  end
endmodule

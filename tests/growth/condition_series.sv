// A condition of N clauses joined by `&&&`, each a bit that cannot be unknown:
// a list the source writes, which a step makes one longer and no deeper (LRM
// 12.6.2).
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.class constants 1, lir.constants 1
module Top #(parameter int N = 4);
  bit [[b{i}|, ]];
  bit held;

  initial begin
    if ([[b{i}| &&& ]]) held = 1;
  end
endmodule

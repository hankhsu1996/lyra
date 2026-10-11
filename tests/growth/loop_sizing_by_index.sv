// A generate loop whose blocks each reduce a part one bit wider than the block
// before reduced (LRM 27.4), so no two blocks do one text's worth of work: each
// is a procedure of its own over a type of its own. N is how many blocks the
// loop counts out.
// @each-adds: mir.callables 1, mir.types 1, lir.functions 1, lir.types 1, bytes
module Top #(parameter int N = 4);
  logic [N-1:0] a;
  logic [N-1:0] y;
  for (genvar i = 0; i < N; i += 1) begin : g
    assign y[i] = ^a[i:0];
  end
endmodule

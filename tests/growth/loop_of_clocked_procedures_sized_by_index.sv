// A generate loop whose blocks each hold a clocked procedure that reduces a
// part one bit wider than the block before reduced (LRM 9.2.2.4, 27.4), so
// each is a procedure of its own over a type of its own, and none of them waits
// anywhere but for its clock. N is how many blocks the loop counts out.
// @each-adds: mir.callables 1, mir.types 1, lir.functions 1, lir.types 1, bytes
module Top #(parameter int N = 4);
  logic clk;
  logic [N-1:0] d;
  logic [N-1:0] q;
  for (genvar i = 0; i < N; i += 1) begin : g
    always_ff @(posedge clk) q[i] <= ^d[i:0];
  end
endmodule

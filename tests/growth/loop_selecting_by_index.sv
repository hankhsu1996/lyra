// A generate loop whose blocks each reach the bit their index selects, which is
// a constant select (LRM 11.5.3) of a different bit in every block and still
// one text. N is how many blocks the loop counts out.
module Top #(parameter int N = 4);
  logic [N-1:0] v;
  logic d [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    assign d[i] = v[i];
  end
endmodule

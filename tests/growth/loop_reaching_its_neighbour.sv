// A generate loop whose blocks each read the variable the block before them
// declares, by a hierarchical name indexed one below their own (LRM 23.6,
// 27.4). Every block past the first is one text reaching a different block. N
// is how many blocks the loop counts out.
module Top #(parameter int N = 4);
  logic [N-1:0] a;
  logic y;
  for (genvar i = 0; i < N; i += 1) begin : g
    logic t;
    if (i == 0) begin : first
      assign t = a[0];
    end else begin : rest
      assign t = g[i-1].t ^ a[i];
    end
  end
  assign y = g[N-1].t;
endmodule

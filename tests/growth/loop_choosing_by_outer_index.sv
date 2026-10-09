// A loop around a loop, the inner blocks choosing an alternative by the outer
// index (LRM 27.5). Every outer block states the same construct with both
// alternatives. N is how many blocks the outer loop counts out.
module Top #(parameter int N = 4);
  int sink [N][4];
  for (genvar g = 0; g < N; g += 1) begin : gg
    for (genvar i = 0; i < 4; i += 1) begin : gi
      if ((g % 2) == 0) begin : t
        initial sink[g][i] = 1;
      end else begin : f
        initial sink[g][i] = 2;
      end
    end
  end
endmodule

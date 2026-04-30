module Top;
  for (genvar i = 0; i < 2; i = i + 1) begin : g
    initial begin
      int x;
      x = i;
    end
  end
endmodule

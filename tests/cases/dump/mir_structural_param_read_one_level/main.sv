module Top;
  for (genvar i = 0; i < 2; i = i + 1) begin : g
    if (1) begin : h
      initial begin
        int x;
        x = i;
      end
    end
  end
endmodule

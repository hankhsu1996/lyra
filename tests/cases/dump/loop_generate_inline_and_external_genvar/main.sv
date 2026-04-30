module Top;
  genvar g;
  for (g = 0; g < 2; g = g + 1) begin : a
    int x;
  end

  for (genvar i = 0; i < 2; i = i + 1) begin : b
    int y;
  end
endmodule

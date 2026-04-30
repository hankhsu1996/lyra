module Top;
  int x;
  if (1) begin : g
    initial begin
      int y;
      y = 5;
      x = y + 1;
    end
  end
endmodule

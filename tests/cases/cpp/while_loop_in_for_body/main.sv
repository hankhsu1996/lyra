module Top;
  int total;
  initial begin
    total = 0;
    for (int row = 0; row < 3; row = row + 1) begin
      int col;
      col = 0;
      while (col < 4) begin
        total = total + 1;
        col = col + 1;
      end
    end
  end
endmodule

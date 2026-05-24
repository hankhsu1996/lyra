module Top;
  initial begin
    int total = 0;
    for (int row = 0; row < 3; row = row + 1) begin
      int col = 0;
      while (col < 4) begin
        total = total + 1;
        col = col + 1;
      end
    end
    $display("total=%0d", total);
  end
endmodule

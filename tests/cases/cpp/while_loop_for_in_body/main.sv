module Top;
  initial begin
    int row = 0;
    int total = 0;
    while (row < 3) begin
      for (int col = 0; col < 4; col = col + 1) begin
        total = total + 1;
      end
      row = row + 1;
    end
    $display("total=%0d", total);
  end
endmodule

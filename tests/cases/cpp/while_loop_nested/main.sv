module Top;
  initial begin
    int i = 0;
    int total = 0;
    while (i < 3) begin
      int j = 0;
      while (j < 4) begin
        total = total + 1;
        j = j + 1;
      end
      i = i + 1;
    end
    $display("total=%0d", total);
  end
endmodule

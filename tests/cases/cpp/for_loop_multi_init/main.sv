module Top;
  initial begin
    int i;
    int j;
    int sum = 0;
    int diff = 0;
    for (i = 0, j = 10; i < 5; i = i + 1, j = j - 1) begin
      sum = sum + i + j;
      diff = diff - 2;
    end
    $display("sum=%0d", sum);
    $display("diff=%0d", diff);
  end
endmodule

module Top;
  initial begin
    int i = 0;
    int sum = 0;
    while (i < 5) begin
      sum = sum + i;
      i = i + 1;
    end
    $display("i=%0d sum=%0d", i, sum);
  end
endmodule

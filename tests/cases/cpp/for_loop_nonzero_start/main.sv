module Top;
  initial begin
    int sum = 0;
    for (int i = 5; i < 10; i = i + 1) sum = sum + i;
    $display("sum=%0d", sum);
  end
endmodule

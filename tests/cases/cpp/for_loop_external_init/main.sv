module Top;
  initial begin
    int i;
    int sum = 0;
    for (i = 0; i < 5; i = i + 1) sum = sum + i;
    $display("sum=%0d", sum);
  end
endmodule

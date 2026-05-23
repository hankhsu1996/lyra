module Top;
  initial begin
    int sum = 99;
    for (int i = 0; i < 0; i = i + 1) sum = sum + 1;
    $display("sum=%0d", sum);
  end
endmodule

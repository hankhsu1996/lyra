module Top;
  int sum;
  initial begin
    sum = 0;
    for (int i = 10; i > 0; i = i - 1) sum = sum + i;
  end
endmodule

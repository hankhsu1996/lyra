module Top;
  int sum;
  initial begin
    sum = 0;
    for (int i = 5; i < 10; i = i + 1) sum = sum + i;
  end
endmodule

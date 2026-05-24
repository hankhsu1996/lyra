module Top;
  int sum;
  initial begin
    sum = 0;
    for (int i = 0; i < 5; i = i + 1) sum = sum + i;
  end
endmodule

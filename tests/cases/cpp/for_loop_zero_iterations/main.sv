module Top;
  int sum;
  initial begin
    sum = 99;
    for (int i = 0; i < 0; i = i + 1) sum = sum + 1;
  end
endmodule

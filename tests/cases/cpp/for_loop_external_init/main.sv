module Top;
  int sum;
  initial begin
    int i;
    sum = 0;
    for (i = 0; i < 5; i = i + 1) sum = sum + i;
  end
endmodule

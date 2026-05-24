module Top;
  int sum;
  int diff;
  initial begin
    int i;
    int j;
    sum = 0;
    diff = 0;
    for (i = 0, j = 10; i < 5; i = i + 1, j = j - 1) begin
      sum = sum + i + j;
      diff = diff - 2;
    end
  end
endmodule

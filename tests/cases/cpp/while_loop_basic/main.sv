module Top;
  int i;
  int sum;
  initial begin
    i = 0;
    sum = 0;
    while (i < 5) begin
      sum = sum + i;
      i = i + 1;
    end
  end
endmodule

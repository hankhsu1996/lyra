module Top;
  int sum;
  initial begin
    int n;
    int i;
    n = 6;
    sum = 0;
    i = 1;
    repeat (n) begin
      sum = sum + i;
      i = i + 1;
    end
  end
endmodule

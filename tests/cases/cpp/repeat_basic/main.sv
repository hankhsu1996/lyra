module Top;
  int sum;
  initial begin
    int i;
    sum = 0;
    i = 0;
    repeat (5) begin
      sum = sum + i;
      i = i + 1;
    end
  end
endmodule

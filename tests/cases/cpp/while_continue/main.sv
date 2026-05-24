module Top;
  int sum;
  initial begin
    int i;
    i = 0;
    sum = 0;
    while (i < 5) begin
      i = i + 1;
      if (i == 2) continue;
      if (i == 4) continue;
      sum = sum + i;
    end
  end
endmodule

module Top;
  int sum;
  initial begin
    sum = 0;
    for (int k = 0; k < 5; k = k + 1) begin
      if (k == 2) continue;
      sum = sum + k;
    end
  end
endmodule

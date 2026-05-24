module Top;
  int sum;
  initial begin
    sum = 0;
    for (int k = 0; k < 10; k = k + 1) begin
      if (k == 5) break;
      if (k == 2) continue;
      sum = sum + k;
    end
  end
endmodule

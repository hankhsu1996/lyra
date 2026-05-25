module Top;
  int i;
  int sum_evens;
  int sum_odds;
  initial begin
    sum_evens = 0;
    sum_odds = 0;
    for (i = 1; i <= 6; i = i + 1) begin
      sum_evens = sum_evens + (((i % 2) == 0) ? i : 0);
      sum_odds = sum_odds + (((i % 2) != 0) ? i : 0);
    end
  end
endmodule

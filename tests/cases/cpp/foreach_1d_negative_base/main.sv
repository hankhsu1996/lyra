module Top;
  int arr [-2:1] = '{10, 20, 30, 40};
  int sum_idx;
  int sum_val;
  int first_idx;
  int last_idx;

  initial begin
    sum_idx = 0;
    sum_val = 0;
    first_idx = 99;
    last_idx = 99;
    foreach (arr[i]) begin
      if (first_idx == 99) first_idx = i;
      last_idx = i;
      sum_idx = sum_idx + i;
      sum_val = sum_val + arr[i];
    end
  end
endmodule

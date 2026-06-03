module Top;
  int arr [5] = '{1, 2, 3, 4, 5};
  int sum_all;
  int sum_break;
  int sum_continue;
  int last_break;

  initial begin
    sum_all = 0;
    sum_break = 0;
    sum_continue = 0;
    last_break = -1;
    foreach (arr[i]) begin
      sum_all = sum_all + arr[i];
    end
    foreach (arr[i]) begin
      if (arr[i] >= 4) break;
      sum_break = sum_break + arr[i];
      last_break = i;
    end
    foreach (arr[i]) begin
      if (arr[i] == 3) continue;
      sum_continue = sum_continue + arr[i];
    end
  end
endmodule

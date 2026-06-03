module Top;
  int arr [2][3][4];
  int count;
  int sum_i;
  int first_i, last_i;

  initial begin
    count = 0;
    sum_i = 0;
    first_i = -1;
    last_i = -1;
    // Only iterate the outermost dimension: 2 iterations.
    foreach (arr[i]) begin
      if (first_i == -1) first_i = i;
      last_i = i;
      sum_i = sum_i + i;
      count = count + 1;
    end
  end
endmodule

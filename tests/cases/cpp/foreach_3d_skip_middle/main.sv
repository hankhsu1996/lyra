module Top;
  int arr [2][3][4];
  int count;
  int first_i, first_k;
  int last_i, last_k;

  initial begin
    count = 0;
    first_i = -1;
    first_k = -1;
    last_i = -1;
    last_k = -1;
    // Skip the middle dimension: iterate only outer (2) x inner (4) = 8 times.
    foreach (arr[i, , k]) begin
      if (first_i == -1) begin
        first_i = i;
        first_k = k;
      end
      last_i = i;
      last_k = k;
      count = count + 1;
    end
  end
endmodule

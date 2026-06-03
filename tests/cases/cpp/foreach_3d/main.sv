module Top;
  int arr [2][3][4];
  int count;
  int first_i, first_j, first_k;
  int last_i, last_j, last_k;

  initial begin
    count = 0;
    first_i = -1;
    first_j = -1;
    first_k = -1;
    last_i = -1;
    last_j = -1;
    last_k = -1;
    foreach (arr[i, j, k]) begin
      if (first_i == -1) begin
        first_i = i;
        first_j = j;
        first_k = k;
      end
      last_i = i;
      last_j = j;
      last_k = k;
      count = count + 1;
    end
  end
endmodule

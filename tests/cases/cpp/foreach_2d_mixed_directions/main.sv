module Top;
  int arr [0:2][5:3];
  int first_i, first_j;
  int last_i, last_j;

  initial begin
    first_i = -1;
    first_j = -1;
    last_i = -1;
    last_j = -1;
    // Outer ascending [0:2], inner descending [5:3]. First iteration is
    // (i=0, j=5), last iteration is (i=2, j=3).
    foreach (arr[i, j]) begin
      if (first_i == -1) begin
        first_i = i;
        first_j = j;
      end
      last_i = i;
      last_j = j;
    end
  end
endmodule

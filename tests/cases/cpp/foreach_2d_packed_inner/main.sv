module Top;
  // Outer: fixed unpacked of size 3. Inner: packed bit-vector [7:0] (descending).
  bit [7:0] arr [3];
  int count;
  int first_i, first_j;
  int last_i, last_j;

  initial begin
    count = 0;
    first_i = -1;
    first_j = -1;
    last_i = -1;
    last_j = -1;
    // Total = 3 * 8 = 24 iterations. j iterates 7 -> 0 (descending packed).
    foreach (arr[i, j]) begin
      if (first_i == -1) begin
        first_i = i;
        first_j = j;
      end
      last_i = i;
      last_j = j;
      count = count + 1;
    end
  end
endmodule

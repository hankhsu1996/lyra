module Top;
  int arr [2][3] = '{'{10, 20, 30}, '{40, 50, 60}};
  int sum;
  int last_i;
  int last_j;

  initial begin
    sum = 0;
    last_i = -1;
    last_j = -1;
    // LRM 12.8: break exits the ENTIRE foreach, not just the innermost dim.
    // Iteration order: (0,0)=10 (sum=10), (0,1)=20 -> break immediately.
    // If break only exited the inner loop, iteration would continue at (1,0),
    // sum would reach 160, last_i/last_j would advance to (1,2).
    foreach (arr[i, j]) begin
      if (arr[i][j] == 20) break;
      sum = sum + arr[i][j];
      last_i = i;
      last_j = j;
    end
  end
endmodule

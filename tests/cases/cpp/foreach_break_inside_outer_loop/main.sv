module Top;
  int arr [3] = '{10, 20, 30};
  int outer_iters;
  int total_sum;

  initial begin
    outer_iters = 0;
    total_sum = 0;
    // foreach is nested inside a while. break in foreach exits only the
    // foreach -- the while continues to its next iteration.
    while (outer_iters < 3) begin
      foreach (arr[i]) begin
        if (arr[i] == 20) break;
        total_sum = total_sum + arr[i];
      end
      outer_iters = outer_iters + 1;
    end
  end
endmodule

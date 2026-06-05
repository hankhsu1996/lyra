module Top;
  int arr [];
  int matrix [][];
  int n_empty, n_three, n_after_writes, n_resized, n_outer;
  initial begin
    // Empty (uncreated) dynamic array reports zero per LRM 7.5.2.
    n_empty = arr.size();
    arr = new[3];
    n_three = arr.size();
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    n_after_writes = arr.size();
    // new[N](src) resizes; size now reflects N.
    arr = new[5](arr);
    n_resized = arr.size();
    // Multi-dim outer-dim count.
    matrix = new[2];
    matrix[0] = new[4];
    matrix[1] = new[4];
    n_outer = matrix.size();
  end
endmodule

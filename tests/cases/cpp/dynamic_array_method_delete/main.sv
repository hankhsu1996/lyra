module Top;
  int arr [];
  int matrix [][];
  int size_after_delete;
  initial begin
    arr = new[3];
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    arr.delete();
    size_after_delete = arr.size();
    // Re-fill after delete: a fresh `new[N]` rebuilds cleanly.
    arr = new[3];
    arr[0] = 7;
    arr[1] = 8;
    arr[2] = 9;
    // Multi-dim outer-dim delete clears the entire row table.
    matrix = new[2];
    matrix[0] = new[3];
    matrix[1] = new[3];
    matrix.delete();
  end
endmodule

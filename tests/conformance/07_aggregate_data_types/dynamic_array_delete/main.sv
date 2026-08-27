// The delete() method empties a dynamic array, leaving one of size zero, so
// every index into it is then out of bounds and a read through one yields the
// element type's default. Emptying an array is not a state it stays in: the
// array can be constructed again afterwards and holds what is written to it.
// The method empties whatever array it is called on, so on the outer
// dimension of a multidimensional dynamic array it discards the subarrays
// that dimension held (LRM 7.5.3, 7.5.2, 7.4.5, Table 7-1).
module Top;
  int filled [] = '{100, 200, 300};
  int already_empty [];
  int matrix [][];

  int size_before;
  int size_after = 77;
  int read_after_delete = 77;

  int size_rebuilt;
  int rebuilt0 = 77;
  int rebuilt2 = 77;

  int size_already_empty = 77;

  int matrix_size_before;
  int matrix_size_after = 77;

  initial begin
    size_before = filled.size();
    filled.delete();
    size_after = filled.size();
    read_after_delete = filled[0];

    filled = new[3];
    filled[0] = 7;
    filled[1] = 8;
    filled[2] = 9;
    size_rebuilt = filled.size();
    rebuilt0 = filled[0];
    rebuilt2 = filled[2];

    already_empty.delete();
    size_already_empty = already_empty.size();

    matrix = new[2];
    matrix[0] = new[3];
    matrix[1] = new[3];
    matrix[0][0] = 1;
    matrix[1][0] = 10;
    matrix_size_before = matrix.size();
    matrix.delete();
    matrix_size_after = matrix.size();
  end

  final begin
    if (size_before !== 3)
      $fatal(1, "size_before was %0d, expected 3", size_before);
    if (size_after !== 0)
      $fatal(1, "size_after was %0d, expected 0", size_after);
    if (read_after_delete !== 0)
      $fatal(1, "read_after_delete was %0d, expected 0", read_after_delete);

    if (size_rebuilt !== 3)
      $fatal(1, "size_rebuilt was %0d, expected 3", size_rebuilt);
    if (rebuilt0 !== 7) $fatal(1, "rebuilt0 was %0d, expected 7", rebuilt0);
    if (rebuilt2 !== 9) $fatal(1, "rebuilt2 was %0d, expected 9", rebuilt2);

    if (size_already_empty !== 0)
      $fatal(1, "size_already_empty was %0d, expected 0", size_already_empty);

    if (matrix_size_before !== 2)
      $fatal(1, "matrix_size_before was %0d, expected 2", matrix_size_before);
    if (matrix_size_after !== 0)
      $fatal(1, "matrix_size_after was %0d, expected 0", matrix_size_after);
    $display("All checks passed");
  end
endmodule

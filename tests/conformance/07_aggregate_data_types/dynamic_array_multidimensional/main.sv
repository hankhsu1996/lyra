// Any unpacked dimension of an array may be a dynamic one, and each dynamic
// dimension is sized on its own. Constructing the outer dimension does not
// construct the subarrays it holds: a dynamic subarray stays empty until it
// is constructed in turn, and the rows of one array may be given different
// sizes. A subarray of fixed size needs no construction and arrives with its
// elements already at the element type's default. An array whose leftmost
// unpacked dimension is fixed is not itself a dynamic array, but its
// subarrays still are and each is constructed on its own (LRM 7.5, 7.5.1,
// Table 7-1).
module Top;
  int jagged [][];
  int fixed_outer [2][];
  int fixed_inner [][3];

  int jagged_size;
  int row0_size_before_new = 77;
  int row1_size_before_new = 77;
  int row0_size;
  int row1_size;
  int jagged00 = 77;
  int jagged12 = 77;

  int fixed_outer_row0_size;
  int fixed_outer_row1_size;
  int fixed_outer01 = 77;
  int fixed_outer10 = 77;

  int fixed_inner_size;
  int fixed_inner02 = 77;
  int fixed_inner10 = 77;

  initial begin
    jagged = new[2];
    jagged_size = jagged.size();
    row0_size_before_new = jagged[0].size();
    row1_size_before_new = jagged[1].size();

    jagged[0] = new[3];
    jagged[1] = new[5];
    row0_size = jagged[0].size();
    row1_size = jagged[1].size();
    jagged[0][0] = 41;
    jagged[1][2] = 42;
    jagged00 = jagged[0][0];
    jagged12 = jagged[1][2];

    fixed_outer[0] = new[4];
    fixed_outer[1] = new[1];
    fixed_outer_row0_size = fixed_outer[0].size();
    fixed_outer_row1_size = fixed_outer[1].size();
    fixed_outer[0][1] = 51;
    fixed_outer[1][0] = 52;
    fixed_outer01 = fixed_outer[0][1];
    fixed_outer10 = fixed_outer[1][0];

    fixed_inner = new[2];
    fixed_inner_size = fixed_inner.size();
    fixed_inner02 = fixed_inner[0][2];
    fixed_inner[1][0] = 61;
    fixed_inner10 = fixed_inner[1][0];
  end

  final begin
    if (jagged_size !== 2)
      $fatal(1, "jagged_size was %0d, expected 2", jagged_size);
    if (row0_size_before_new !== 0)
      $fatal(1, "row0_size_before_new was %0d, expected 0",
             row0_size_before_new);
    if (row1_size_before_new !== 0)
      $fatal(1, "row1_size_before_new was %0d, expected 0",
             row1_size_before_new);
    if (row0_size !== 3)
      $fatal(1, "row0_size was %0d, expected 3", row0_size);
    if (row1_size !== 5)
      $fatal(1, "row1_size was %0d, expected 5", row1_size);
    if (jagged00 !== 41) $fatal(1, "jagged00 was %0d, expected 41", jagged00);
    if (jagged12 !== 42) $fatal(1, "jagged12 was %0d, expected 42", jagged12);

    if (fixed_outer_row0_size !== 4)
      $fatal(1, "fixed_outer_row0_size was %0d, expected 4",
             fixed_outer_row0_size);
    if (fixed_outer_row1_size !== 1)
      $fatal(1, "fixed_outer_row1_size was %0d, expected 1",
             fixed_outer_row1_size);
    if (fixed_outer01 !== 51)
      $fatal(1, "fixed_outer01 was %0d, expected 51", fixed_outer01);
    if (fixed_outer10 !== 52)
      $fatal(1, "fixed_outer10 was %0d, expected 52", fixed_outer10);

    if (fixed_inner_size !== 2)
      $fatal(1, "fixed_inner_size was %0d, expected 2", fixed_inner_size);
    if (fixed_inner02 !== 0)
      $fatal(1, "fixed_inner02 was %0d, expected 0", fixed_inner02);
    if (fixed_inner10 !== 61)
      $fatal(1, "fixed_inner10 was %0d, expected 61", fixed_inner10);
    $display("All checks passed");
  end
endmodule

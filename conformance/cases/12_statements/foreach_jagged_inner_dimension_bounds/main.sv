// Several loop variables in a foreach-loop correspond to nested loops, so an
// inner loop variable ranges over the sub-array the outer loop variables have
// already selected rather than over one bound fixed for the whole traversal.
// When the rows of a dynamically sized array have been given different
// lengths, each row is therefore walked over its own length, and a dynamic
// dimension may sit on either side of a fixed one (LRM 12.7.3).
module Top;
  int jagged [][];
  int jagged_sum;
  int jagged_passes;

  int fixed_inner [][2];
  int fixed_inner_sum;

  int fixed_outer [3][];
  int fixed_outer_sum;

  initial begin
    jagged = new[3];
    jagged[0] = '{1, 2};
    jagged[1] = '{3, 4, 5, 6};
    jagged[2] = '{7};
    jagged_sum = 0;
    foreach (jagged[i, j]) jagged_sum = jagged_sum + jagged[i][j];
    jagged_passes = 0;
    foreach (jagged[i, j]) begin
      jagged[i][j] = jagged_passes;
      jagged_passes = jagged_passes + 1;
    end

    fixed_inner = new[2];
    fixed_inner[0] = '{10, 20};
    fixed_inner[1] = '{30, 40};
    fixed_inner_sum = 0;
    foreach (fixed_inner[i, j])
      fixed_inner_sum = fixed_inner_sum + fixed_inner[i][j];

    fixed_outer[0] = '{1, 2};
    fixed_outer[1] = '{10, 20, 30};
    fixed_outer[2] = '{100};
    fixed_outer_sum = 0;
    foreach (fixed_outer[i, j])
      fixed_outer_sum = fixed_outer_sum + fixed_outer[i][j];
  end

  final begin
    if (jagged_sum !== 28)
      $fatal(1, "jagged_sum was %0d, expected 28", jagged_sum);
    if (jagged_passes !== 7)
      $fatal(1, "jagged_passes was %0d, expected 7", jagged_passes);
    if (jagged[0][0] !== 0)
      $fatal(1, "jagged[0][0] was %0d, expected 0", jagged[0][0]);
    if (jagged[0][1] !== 1)
      $fatal(1, "jagged[0][1] was %0d, expected 1", jagged[0][1]);
    if (jagged[1][0] !== 2)
      $fatal(1, "jagged[1][0] was %0d, expected 2", jagged[1][0]);
    if (jagged[1][3] !== 5)
      $fatal(1, "jagged[1][3] was %0d, expected 5", jagged[1][3]);
    if (jagged[2][0] !== 6)
      $fatal(1, "jagged[2][0] was %0d, expected 6", jagged[2][0]);
    if (fixed_inner_sum !== 100)
      $fatal(1, "fixed_inner_sum was %0d, expected 100", fixed_inner_sum);
    if (fixed_outer_sum !== 163)
      $fatal(1, "fixed_outer_sum was %0d, expected 163", fixed_outer_sum);
    $display("All checks passed");
  end
endmodule

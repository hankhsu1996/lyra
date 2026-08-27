// A foreach-loop's list of loop variables may name fewer variables than the
// array has dimensions. A variable omitted from a position in the list means
// no iteration over the dimension that position stands for, and trailing
// commas may be left out of the list, which leaves only the leading dimensions
// iterated. The variables that remain keep the dimensions their positions
// name, so the number of passes is the product of the sizes of the iterated
// dimensions alone (LRM 12.7.3).
module Top;
  int cube [2][3][4];

  int skip_middle_visits [8];
  int skip_middle_passes;

  int skip_outer_passes;
  int skip_outer_index_sum;

  int leading_two_passes;
  int leading_one_passes;
  int leading_one_index_sum;

  initial begin
    skip_middle_passes = 0;
    foreach (cube[i, , k]) begin
      skip_middle_visits[skip_middle_passes] = i * 10 + k;
      skip_middle_passes = skip_middle_passes + 1;
    end

    skip_outer_passes = 0;
    skip_outer_index_sum = 0;
    foreach (cube[, j]) begin
      skip_outer_passes = skip_outer_passes + 1;
      skip_outer_index_sum = skip_outer_index_sum + j;
    end

    leading_two_passes = 0;
    foreach (cube[i, j]) leading_two_passes = leading_two_passes + 1;

    leading_one_passes = 0;
    leading_one_index_sum = 0;
    foreach (cube[i]) begin
      leading_one_passes = leading_one_passes + 1;
      leading_one_index_sum = leading_one_index_sum + i;
    end
  end

  final begin
    if (skip_middle_passes !== 8)
      $fatal(1, "skip_middle_passes was %0d, expected 8", skip_middle_passes);
    if (skip_middle_visits[0] !== 0)
      $fatal(1, "skip_middle_visits[0] was %0d, expected 0",
             skip_middle_visits[0]);
    if (skip_middle_visits[3] !== 3)
      $fatal(1, "skip_middle_visits[3] was %0d, expected 3",
             skip_middle_visits[3]);
    if (skip_middle_visits[4] !== 10)
      $fatal(1, "skip_middle_visits[4] was %0d, expected 10",
             skip_middle_visits[4]);
    if (skip_middle_visits[7] !== 13)
      $fatal(1, "skip_middle_visits[7] was %0d, expected 13",
             skip_middle_visits[7]);
    if (skip_outer_passes !== 3)
      $fatal(1, "skip_outer_passes was %0d, expected 3", skip_outer_passes);
    if (skip_outer_index_sum !== 3)
      $fatal(1, "skip_outer_index_sum was %0d, expected 3",
             skip_outer_index_sum);
    if (leading_two_passes !== 6)
      $fatal(1, "leading_two_passes was %0d, expected 6", leading_two_passes);
    if (leading_one_passes !== 2)
      $fatal(1, "leading_one_passes was %0d, expected 2", leading_one_passes);
    if (leading_one_index_sum !== 1)
      $fatal(1, "leading_one_index_sum was %0d, expected 1",
             leading_one_index_sum);
    $display("All checks passed");
  end
endmodule

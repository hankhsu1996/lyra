// A foreach-loop variable takes the values of the dimension's declared index
// range in declared order, the left bound first and the right bound last. An
// ascending range therefore counts up and a descending one counts down, and
// the range need neither begin at zero nor stay positive; the standard's own
// example walks a [5:1] dimension from 5 down to 1. Each dimension of a
// multidimensional array keeps its own direction (LRM 12.7.3).
module Top;
  int neg_base [-2:1] = '{1, 2, 3, 4};
  int neg_base_sum;
  int neg_base_index_sum;
  int neg_base_passes;

  bit [3:0] packed_desc = 4'b1010;
  int packed_index_order;
  int packed_bit_order;
  int packed_set_count;

  int mixed_dirs [0:2][5:3];
  int mixed_passes;

  initial begin
    neg_base_sum = 0;
    neg_base_index_sum = 0;
    foreach (neg_base[i]) begin
      neg_base_sum = neg_base_sum + neg_base[i];
      neg_base_index_sum = neg_base_index_sum + i;
    end
    neg_base_passes = 0;
    foreach (neg_base[i]) begin
      neg_base[i] = neg_base_passes;
      neg_base_passes = neg_base_passes + 1;
    end

    packed_index_order = 0;
    packed_bit_order = 0;
    packed_set_count = 0;
    foreach (packed_desc[i]) begin
      packed_index_order = packed_index_order * 10 + i;
      packed_bit_order = packed_bit_order * 10 + packed_desc[i];
      if (packed_desc[i]) packed_set_count = packed_set_count + 1;
    end

    mixed_passes = 0;
    foreach (mixed_dirs[i, j]) begin
      mixed_dirs[i][j] = mixed_passes;
      mixed_passes = mixed_passes + 1;
    end
  end

  final begin
    if (neg_base_sum !== 10)
      $fatal(1, "neg_base_sum was %0d, expected 10", neg_base_sum);
    if (neg_base_index_sum !== -2)
      $fatal(1, "neg_base_index_sum was %0d, expected -2",
             neg_base_index_sum);
    if (neg_base_passes !== 4)
      $fatal(1, "neg_base_passes was %0d, expected 4", neg_base_passes);
    if (neg_base[-2] !== 0)
      $fatal(1, "neg_base[-2] was %0d, expected 0", neg_base[-2]);
    if (neg_base[-1] !== 1)
      $fatal(1, "neg_base[-1] was %0d, expected 1", neg_base[-1]);
    if (neg_base[0] !== 2)
      $fatal(1, "neg_base[0] was %0d, expected 2", neg_base[0]);
    if (neg_base[1] !== 3)
      $fatal(1, "neg_base[1] was %0d, expected 3", neg_base[1]);
    if (packed_index_order !== 3210)
      $fatal(1, "packed_index_order was %0d, expected 3210",
             packed_index_order);
    if (packed_bit_order !== 1010)
      $fatal(1, "packed_bit_order was %0d, expected 1010", packed_bit_order);
    if (packed_set_count !== 2)
      $fatal(1, "packed_set_count was %0d, expected 2", packed_set_count);
    if (mixed_passes !== 9)
      $fatal(1, "mixed_passes was %0d, expected 9", mixed_passes);
    if (mixed_dirs[0][5] !== 0)
      $fatal(1, "mixed_dirs[0][5] was %0d, expected 0", mixed_dirs[0][5]);
    if (mixed_dirs[0][4] !== 1)
      $fatal(1, "mixed_dirs[0][4] was %0d, expected 1", mixed_dirs[0][4]);
    if (mixed_dirs[0][3] !== 2)
      $fatal(1, "mixed_dirs[0][3] was %0d, expected 2", mixed_dirs[0][3]);
    if (mixed_dirs[1][5] !== 3)
      $fatal(1, "mixed_dirs[1][5] was %0d, expected 3", mixed_dirs[1][5]);
    if (mixed_dirs[2][3] !== 8)
      $fatal(1, "mixed_dirs[2][3] was %0d, expected 8", mixed_dirs[2][3]);
    $display("All checks passed");
  end
endmodule

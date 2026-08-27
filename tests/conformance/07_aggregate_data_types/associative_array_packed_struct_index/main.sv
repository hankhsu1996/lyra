// A packed struct is an integral type for which equality is defined, so it is a
// legal associative array index type: two struct values index the same entry
// exactly when their bit patterns are equal, and the array orders its entries
// by the numerical value of those patterns (LRM 7.8.5, 7.8.4).
module Top;
  typedef struct packed {
    bit [7:0] high;
    bit [7:0] low;
  } Pair;

  int by_pair [Pair];

  Pair one_two;
  Pair one_three;
  Pair zero_nine;
  Pair same_as_one_two;

  int value_one_two;
  int value_one_three;
  int value_via_equal_pattern;
  int count_after_writes;
  int exists_present;
  int exists_deleted;
  int count_after_delete;

  Pair smallest_key;
  Pair largest_key;

  initial begin
    one_two = '{high: 8'd1, low: 8'd2};
    one_three = '{high: 8'd1, low: 8'd3};
    zero_nine = '{high: 8'd0, low: 8'd9};
    same_as_one_two = '{high: 8'd1, low: 8'd2};

    by_pair[one_two] = 42;
    by_pair[one_three] = 43;
    by_pair[zero_nine] = 44;

    value_one_two = by_pair[one_two];
    value_one_three = by_pair[one_three];
    value_via_equal_pattern = by_pair[same_as_one_two];
    count_after_writes = by_pair.num();
    exists_present = by_pair.exists(one_two);

    void'(by_pair.first(smallest_key));
    void'(by_pair.last(largest_key));

    by_pair.delete(one_two);
    exists_deleted = by_pair.exists(one_two);
    count_after_delete = by_pair.num();
  end

  final begin
    if (value_one_two !== 42)
      $fatal(1, "value_one_two was %0d, expected 42", value_one_two);
    if (value_one_three !== 43)
      $fatal(1, "value_one_three was %0d, expected 43", value_one_three);
    if (value_via_equal_pattern !== 42)
      $fatal(1, "value_via_equal_pattern was %0d, expected 42",
             value_via_equal_pattern);
    if (count_after_writes !== 3)
      $fatal(1, "count_after_writes was %0d, expected 3", count_after_writes);
    if (exists_present !== 1)
      $fatal(1, "exists_present was %0d, expected 1", exists_present);

    if (smallest_key.high !== 8'd0)
      $fatal(1, "smallest_key.high was %0d, expected 0", smallest_key.high);
    if (smallest_key.low !== 8'd9)
      $fatal(1, "smallest_key.low was %0d, expected 9", smallest_key.low);
    if (largest_key.high !== 8'd1)
      $fatal(1, "largest_key.high was %0d, expected 1", largest_key.high);
    if (largest_key.low !== 8'd3)
      $fatal(1, "largest_key.low was %0d, expected 3", largest_key.low);

    if (exists_deleted !== 0)
      $fatal(1, "exists_deleted was %0d, expected 0", exists_deleted);
    if (count_after_delete !== 2)
      $fatal(1, "count_after_delete was %0d, expected 2", count_after_delete);
    $display("All checks passed");
  end
endmodule

// A case statement written with inside compares the case expression against
// each item's range list with the set membership operator, taking the first
// item that matches and the default item when none does (LRM 12.5.4). A range
// list member may be a value the expression must equal, or a [low:high] range
// the expression must lie inclusively within (LRM 11.4.13).
module Top;
  int val;
  int single_value;
  int value_list;
  int at_range_low;
  int within_range;
  int at_range_high;
  int overlapping_ranges;
  int mixed_list;
  int outside_ranges;

  initial begin
    val = 2;
    single_value = 0;
    case (val) inside
      1: single_value = 1;
      2: single_value = 2;
      3: single_value = 3;
    endcase

    val = 3;
    value_list = 0;
    case (val) inside
      1, 2, 3: value_list = 23;
      4, 5:    value_list = 45;
    endcase

    val = 1;
    at_range_low = 0;
    case (val) inside
      [1:3]: at_range_low = 1;
      [4:6]: at_range_low = 2;
      [7:9]: at_range_low = 3;
    endcase

    val = 5;
    within_range = 0;
    case (val) inside
      [1:3]: within_range = 1;
      [4:6]: within_range = 2;
      [7:9]: within_range = 3;
    endcase

    val = 9;
    at_range_high = 0;
    case (val) inside
      [1:3]: at_range_high = 1;
      [4:6]: at_range_high = 2;
      [7:9]: at_range_high = 3;
    endcase

    // The value falls inside more than one item's range list, and the item
    // that runs is the first of them.
    val = 5;
    overlapping_ranges = 0;
    case (val) inside
      [4:6]:   overlapping_ranges = 1;
      [1:9]:   overlapping_ranges = 2;
      5:       overlapping_ranges = 3;
      default: overlapping_ranges = 9;
    endcase

    val = 7;
    mixed_list = 0;
    case (val) inside
      1, 2, 3: mixed_list = 10;
      [4:6]:   mixed_list = 20;
      7:       mixed_list = 30;
      [10:20]: mixed_list = 40;
      default: mixed_list = 99;
    endcase

    val = 100;
    outside_ranges = 0;
    case (val) inside
      [1:3]:   outside_ranges = 1;
      [4:6]:   outside_ranges = 2;
      [7:9]:   outside_ranges = 3;
      default: outside_ranges = 9;
    endcase
  end

  final begin
    if (single_value !== 2)
      $fatal(1, "single_value was %0d, expected 2", single_value);
    if (value_list !== 23)
      $fatal(1, "value_list was %0d, expected 23", value_list);
    if (at_range_low !== 1)
      $fatal(1, "at_range_low was %0d, expected 1", at_range_low);
    if (within_range !== 2)
      $fatal(1, "within_range was %0d, expected 2", within_range);
    if (at_range_high !== 3)
      $fatal(1, "at_range_high was %0d, expected 3", at_range_high);
    if (overlapping_ranges !== 1)
      $fatal(1, "overlapping_ranges was %0d, expected 1", overlapping_ranges);
    if (mixed_list !== 30)
      $fatal(1, "mixed_list was %0d, expected 30", mixed_list);
    if (outside_ranges !== 9)
      $fatal(1, "outside_ranges was %0d, expected 9", outside_ranges);
    $display("All checks passed");
  end
endmodule

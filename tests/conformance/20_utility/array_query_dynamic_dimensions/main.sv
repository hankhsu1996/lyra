// A queue or dynamic array dimension always reports $left 0 and $increment -1,
// whatever it holds; only its extent follows the current state, so $right and
// $high are the last position and are -1 while it is empty. An associative
// dimension takes its index space from its index type -- $left 0 and $right
// that type's highest value -- while $low and $high are the smallest and
// largest indices currently allocated, and read as x when none is (LRM 20.7).
module Top;
  int elements[];
  int items[$];
  int table_by_key[int];
  int table_by_byte[logic [7:0]];
  int rows[][5];

  int dynamic_size;
  int dynamic_left;
  int dynamic_right;
  int dynamic_low;
  int dynamic_high;
  int dynamic_increment;

  int queue_size;
  int queue_right;
  int queue_high;

  int empty_size;
  int empty_right;
  int empty_high;

  int key_size;
  int key_left;
  int key_right;
  int key_low;
  int key_high;
  int key_increment;

  logic [7:0] byte_low;
  logic [7:0] byte_high;
  logic [7:0] byte_right;
  logic [7:0] unallocated_low;
  logic [7:0] unallocated_high;

  int dimension;
  integer rows_outer;
  integer rows_middle;
  integer rows_element;
  integer rows_out_of_range;

  initial begin
    elements = new[4];
    items.push_back(10);
    items.push_back(20);
    items.push_back(30);

    dynamic_size = $size(elements);
    dynamic_left = $left(elements);
    dynamic_right = $right(elements);
    dynamic_low = $low(elements);
    dynamic_high = $high(elements);
    dynamic_increment = $increment(elements);

    queue_size = $size(items);
    queue_right = $right(items);
    queue_high = $high(items);

    items.delete();
    empty_size = $size(items);
    empty_right = $right(items);
    empty_high = $high(items);

    table_by_key[10] = 1;
    table_by_key[-3] = 2;
    table_by_key[7] = 3;
    key_size = $size(table_by_key);
    key_left = $left(table_by_key);
    key_right = $right(table_by_key);
    key_low = $low(table_by_key);
    key_high = $high(table_by_key);
    key_increment = $increment(table_by_key);

    table_by_byte[8'hA0] = 1;
    table_by_byte[8'h0F] = 2;
    byte_low = $low(table_by_byte);
    byte_high = $high(table_by_byte);
    byte_right = $right(table_by_byte);

    table_by_byte.delete();
    unallocated_low = $low(table_by_byte);
    unallocated_high = $high(table_by_byte);

    rows = new[3];
    dimension = 1;
    rows_outer = $size(rows, dimension);
    dimension = 2;
    rows_middle = $size(rows, dimension);
    dimension = 3;
    rows_element = $size(rows, dimension);
    dimension = 4;
    rows_out_of_range = $size(rows, dimension);
  end

  final begin
    if (dynamic_size !== 4)
      $fatal(1, "dynamic_size was %0d, expected 4", dynamic_size);
    if (dynamic_left !== 0)
      $fatal(1, "dynamic_left was %0d, expected 0", dynamic_left);
    if (dynamic_right !== 3)
      $fatal(1, "dynamic_right was %0d, expected 3", dynamic_right);
    if (dynamic_low !== 0)
      $fatal(1, "dynamic_low was %0d, expected 0", dynamic_low);
    if (dynamic_high !== 3)
      $fatal(1, "dynamic_high was %0d, expected 3", dynamic_high);
    if (dynamic_increment !== -1)
      $fatal(1, "dynamic_increment was %0d, expected -1", dynamic_increment);

    if (queue_size !== 3)
      $fatal(1, "queue_size was %0d, expected 3", queue_size);
    if (queue_right !== 2)
      $fatal(1, "queue_right was %0d, expected 2", queue_right);
    if (queue_high !== 2)
      $fatal(1, "queue_high was %0d, expected 2", queue_high);

    if (empty_size !== 0)
      $fatal(1, "empty_size was %0d, expected 0", empty_size);
    if (empty_right !== -1)
      $fatal(1, "empty_right was %0d, expected -1", empty_right);
    if (empty_high !== -1)
      $fatal(1, "empty_high was %0d, expected -1", empty_high);

    if (key_size !== 3) $fatal(1, "key_size was %0d, expected 3", key_size);
    if (key_left !== 0) $fatal(1, "key_left was %0d, expected 0", key_left);
    if (key_right !== 2147483647)
      $fatal(1, "key_right was %0d, expected the highest int", key_right);
    if (key_low !== -3) $fatal(1, "key_low was %0d, expected -3", key_low);
    if (key_high !== 10) $fatal(1, "key_high was %0d, expected 10", key_high);
    if (key_increment !== -1)
      $fatal(1, "key_increment was %0d, expected -1", key_increment);

    if (byte_low !== 8'h0F)
      $fatal(1, "byte_low was %h, expected 0f", byte_low);
    if (byte_high !== 8'hA0)
      $fatal(1, "byte_high was %h, expected a0", byte_high);
    if (byte_right !== 8'hFF)
      $fatal(1, "byte_right was %h, expected ff", byte_right);
    if (unallocated_low !== 8'bx)
      $fatal(1, "unallocated_low was %b, expected x", unallocated_low);
    if (unallocated_high !== 8'bx)
      $fatal(1, "unallocated_high was %b, expected x", unallocated_high);

    if (rows_outer !== 3)
      $fatal(1, "rows_outer was %0d, expected 3", rows_outer);
    if (rows_middle !== 5)
      $fatal(1, "rows_middle was %0d, expected 5", rows_middle);
    if (rows_element !== 32)
      $fatal(1, "rows_element was %0d, expected 32", rows_element);
    if (rows_out_of_range !== 32'bx)
      $fatal(1, "a query for a dimension the type lacks was %b, expected x",
             rows_out_of_range);
    $display("All checks passed");
  end
endmodule

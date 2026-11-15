// An array query function returns an integer (LRM 20.7), whatever the index
// type of the associative array it is asked about. Over an associative
// dimension $left is 0, $increment is -1 and $size counts the indices
// allocated, so sixteen indices of a four-bit index type count as 16. A
// dimension below the associative one is a fixed one and answers as any fixed
// dimension does, whether the dimension is written as a constant or computed
// by the program. An index the integer holds is returned as its own value, so
// a negative index stays negative, an unsigned one stays positive, and a
// member of an enumeration is its value.
module Top;
  typedef enum logic [1:0] {RED, GREEN, BLUE} color_t;

  int narrow[bit [3:0]];
  int by_color[color_t];
  int by_byte[bit [7:0]];
  int signed_keys[byte];
  int rows[bit [7:0]][0:4];

  integer narrow_size;
  integer narrow_high;
  integer byte_left;
  integer byte_right;
  integer byte_increment;
  integer rows_increment;
  integer rows_size;
  integer rows_left;
  integer rows_right;
  integer chosen_increment;
  integer chosen_size;
  integer signed_low;
  integer signed_high;
  integer color_low;
  int dimension;

  initial begin
    for (int i = 0; i < 16; i++) narrow[i[3:0]] = i;
    narrow_size = $size(narrow);
    narrow_high = $high(narrow);

    by_byte[8'h80] = 1;
    byte_left = $left(by_byte);
    byte_right = $right(by_byte);
    byte_increment = $increment(by_byte);

    rows[8'h01] = '{1, 2, 3, 4, 5};
    rows_increment = $increment(rows, 2);
    rows_size = $size(rows, 2);
    rows_left = $left(rows, 2);
    rows_right = $right(rows, 2);

    dimension = 2;
    chosen_increment = $increment(rows, dimension);
    dimension = 1;
    chosen_size = $size(rows, dimension);

    signed_keys[-3] = 1;
    signed_keys[7] = 2;
    signed_low = $low(signed_keys);
    signed_high = $high(signed_keys);

    by_color[BLUE] = 1;
    by_color[GREEN] = 2;
    color_low = $low(by_color);
  end

  final begin
    if (narrow_size !== 16)
      $fatal(1, "$size of sixteen indices was %0d, expected 16", narrow_size);
    if (narrow_high !== 15)
      $fatal(1, "$high of a four-bit index was %0d, expected 15", narrow_high);
    if (byte_left !== 0)
      $fatal(1, "$left of an associative dimension was %0d, expected 0",
             byte_left);
    if (byte_right !== 255)
      $fatal(1, "$right of an eight-bit index was %0d, expected 255",
             byte_right);
    if (byte_increment !== -1)
      $fatal(1, "$increment of an associative dimension was %0d, expected -1",
             byte_increment);
    if (rows_increment !== -1)
      $fatal(1, "$increment of the fixed [0:4] below was %0d, expected -1",
             rows_increment);
    if (rows_size !== 5)
      $fatal(1, "$size of the fixed [0:4] below was %0d, expected 5",
             rows_size);
    if (rows_left !== 0)
      $fatal(1, "$left of the fixed [0:4] below was %0d, expected 0",
             rows_left);
    if (rows_right !== 4)
      $fatal(1, "$right of the fixed [0:4] below was %0d, expected 4",
             rows_right);
    if (chosen_increment !== -1)
      $fatal(1, "$increment of a computed dimension 2 was %0d, expected -1",
             chosen_increment);
    if (chosen_size !== 1)
      $fatal(1, "$size of a computed dimension 1 was %0d, expected 1",
             chosen_size);
    if (signed_low !== -3)
      $fatal(1, "$low of a signed index was %0d, expected -3", signed_low);
    if (signed_high !== 7)
      $fatal(1, "$high of a signed index was %0d, expected 7", signed_high);
    if (color_low !== 1)
      $fatal(1, "$low of an enumerated index was %0d, expected 1", color_low);
    $display("All checks passed");
  end
endmodule

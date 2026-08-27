// The array query functions report a dimension of the operand's type. The
// slowest varying dimension is 1 and the optional second argument names a
// deeper one. $increment is 1 when the left bound is at or above the right one
// and -1 otherwise; $low and $high follow it, and $size is $high - $low + 1.
// An integer type of predefined width counts as one packed dimension of that
// width, a string counts as one dimension that is not unpacked, and a
// dimension index the type has no dimension for reads as x (LRM 20.7).
module Top;
  typedef logic [16:1] Word;

  Word ram[0:9];
  logic [3:0][7:0] packed_two_dimensional;
  int descending[7:2];
  int plain;
  string text;

  int ram_dimensions;
  int ram_unpacked_dimensions;
  int ram_left;
  int ram_right;
  int ram_low;
  int ram_high;
  int ram_increment;
  int ram_size;

  int word_size_by_type;
  int word_size;
  int word_left;
  int word_right;
  int word_low;
  int word_high;
  int word_increment;

  int packed_dimensions;
  int packed_unpacked_dimensions;
  int packed_size;
  int packed_inner_size;
  int packed_left;
  int packed_increment;

  int descending_size;
  int descending_left;
  int descending_right;
  int descending_low;
  int descending_high;
  int descending_increment;
  int descending_dimensions;
  int descending_unpacked_dimensions;

  int plain_size;
  int plain_left;
  int plain_dimensions;

  int text_dimensions;
  int text_unpacked_dimensions;

  int index;
  integer runtime_size;
  integer runtime_left;
  integer runtime_increment;
  integer runtime_out_of_range;

  initial begin
    ram_dimensions = $dimensions(ram);
    ram_unpacked_dimensions = $unpacked_dimensions(ram);
    ram_left = $left(ram);
    ram_right = $right(ram);
    ram_low = $low(ram);
    ram_high = $high(ram);
    ram_increment = $increment(ram);
    ram_size = $size(ram);

    word_size_by_type = $size(Word);
    word_size = $size(ram, 2);
    word_left = $left(ram, 2);
    word_right = $right(ram, 2);
    word_low = $low(ram, 2);
    word_high = $high(ram, 2);
    word_increment = $increment(ram, 2);

    packed_dimensions = $dimensions(packed_two_dimensional);
    packed_unpacked_dimensions =
        $unpacked_dimensions(packed_two_dimensional);
    packed_size = $size(packed_two_dimensional);
    packed_inner_size = $size(packed_two_dimensional, 2);
    packed_left = $left(packed_two_dimensional);
    packed_increment = $increment(packed_two_dimensional);

    descending_size = $size(descending);
    descending_left = $left(descending);
    descending_right = $right(descending);
    descending_low = $low(descending);
    descending_high = $high(descending);
    descending_increment = $increment(descending);
    descending_dimensions = $dimensions(descending);
    descending_unpacked_dimensions = $unpacked_dimensions(descending);

    plain_size = $size(plain);
    plain_left = $left(plain);
    plain_dimensions = $dimensions(plain);

    text_dimensions = $dimensions(text);
    text_unpacked_dimensions = $unpacked_dimensions(text);

    index = 2;
    runtime_size = $size(ram, index);
    runtime_left = $left(ram, index);
    runtime_increment = $increment(ram, index);
    index = 5;
    runtime_out_of_range = $size(ram, index);
  end

  final begin
    if (ram_dimensions !== 2)
      $fatal(1, "ram_dimensions was %0d, expected 2", ram_dimensions);
    if (ram_unpacked_dimensions !== 1)
      $fatal(1, "ram_unpacked_dimensions was %0d, expected 1",
             ram_unpacked_dimensions);
    if (ram_left !== 0) $fatal(1, "ram_left was %0d, expected 0", ram_left);
    if (ram_right !== 9) $fatal(1, "ram_right was %0d, expected 9", ram_right);
    if (ram_low !== 0) $fatal(1, "ram_low was %0d, expected 0", ram_low);
    if (ram_high !== 9) $fatal(1, "ram_high was %0d, expected 9", ram_high);
    if (ram_increment !== -1)
      $fatal(1, "ram_increment was %0d, expected -1", ram_increment);
    if (ram_size !== 10) $fatal(1, "ram_size was %0d, expected 10", ram_size);

    if (word_size_by_type !== 16)
      $fatal(1, "$size of the element type was %0d, expected 16",
             word_size_by_type);
    if (word_size !== 16)
      $fatal(1, "word_size was %0d, expected 16", word_size);
    if (word_left !== 16)
      $fatal(1, "word_left was %0d, expected 16", word_left);
    if (word_right !== 1)
      $fatal(1, "word_right was %0d, expected 1", word_right);
    if (word_low !== 1) $fatal(1, "word_low was %0d, expected 1", word_low);
    if (word_high !== 16)
      $fatal(1, "word_high was %0d, expected 16", word_high);
    if (word_increment !== 1)
      $fatal(1, "word_increment was %0d, expected 1", word_increment);

    if (packed_dimensions !== 2)
      $fatal(1, "packed_dimensions was %0d, expected 2", packed_dimensions);
    if (packed_unpacked_dimensions !== 0)
      $fatal(1, "packed_unpacked_dimensions was %0d, expected 0",
             packed_unpacked_dimensions);
    if (packed_size !== 4)
      $fatal(1, "packed_size was %0d, expected 4", packed_size);
    if (packed_inner_size !== 8)
      $fatal(1, "packed_inner_size was %0d, expected 8", packed_inner_size);
    if (packed_left !== 3)
      $fatal(1, "packed_left was %0d, expected 3", packed_left);
    if (packed_increment !== 1)
      $fatal(1, "packed_increment was %0d, expected 1", packed_increment);

    if (descending_size !== 6)
      $fatal(1, "descending_size was %0d, expected 6", descending_size);
    if (descending_left !== 7)
      $fatal(1, "descending_left was %0d, expected 7", descending_left);
    if (descending_right !== 2)
      $fatal(1, "descending_right was %0d, expected 2", descending_right);
    if (descending_low !== 2)
      $fatal(1, "descending_low was %0d, expected 2", descending_low);
    if (descending_high !== 7)
      $fatal(1, "descending_high was %0d, expected 7", descending_high);
    if (descending_increment !== 1)
      $fatal(1, "descending_increment was %0d, expected 1",
             descending_increment);
    if (descending_dimensions !== 2)
      $fatal(1, "descending_dimensions was %0d, expected 2",
             descending_dimensions);
    if (descending_unpacked_dimensions !== 1)
      $fatal(1, "descending_unpacked_dimensions was %0d, expected 1",
             descending_unpacked_dimensions);

    if (plain_size !== 32)
      $fatal(1, "$size of an int was %0d, expected 32", plain_size);
    if (plain_left !== 31)
      $fatal(1, "$left of an int was %0d, expected 31", plain_left);
    if (plain_dimensions !== 1)
      $fatal(1, "$dimensions of an int was %0d, expected 1", plain_dimensions);

    if (text_dimensions !== 1)
      $fatal(1, "$dimensions of a string was %0d, expected 1",
             text_dimensions);
    if (text_unpacked_dimensions !== 0)
      $fatal(1, "$unpacked_dimensions of a string was %0d, expected 0",
             text_unpacked_dimensions);

    if (runtime_size !== 16)
      $fatal(1, "runtime_size was %0d, expected 16", runtime_size);
    if (runtime_left !== 16)
      $fatal(1, "runtime_left was %0d, expected 16", runtime_left);
    if (runtime_increment !== 1)
      $fatal(1, "runtime_increment was %0d, expected 1", runtime_increment);
    if (runtime_out_of_range !== 32'bx)
      $fatal(1, "a query for a dimension the type lacks was %b, expected x",
             runtime_out_of_range);
    $display("All checks passed");
  end
endmodule

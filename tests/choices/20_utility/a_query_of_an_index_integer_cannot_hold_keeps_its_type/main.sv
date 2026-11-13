// LRM 20.7 types an array query as integer and has $low, $high and $right
// return an index, which an integer cannot hold where the index type is wider
// than 32 bits or is an unsigned 32-bit one. Lyra returns those three at the
// index type there, so the index keeps its value, and an index type that holds
// no x answers its default, 0, for an array holding none. Every other query of
// such an array is still an integer. This case holds Lyra to that.
module Top;
  int wide[longint];
  int unsigned_keys[bit [31:0]];
  int dimension;

  longint wide_low;
  longint wide_low_empty;
  logic [31:0] unsigned_right;
  logic [31:0] unsigned_high;
  integer wide_size;
  integer wide_increment;
  longint chosen_high;

  initial begin
    wide_low_empty = $low(wide);
    wide[64'sh1_0000_0000] = 1;
    wide[-64'sh2_0000_0000] = 2;
    wide_low = $low(wide);
    wide_size = $size(wide);
    wide_increment = $increment(wide);
    dimension = 1;
    chosen_high = $high(wide, dimension);

    unsigned_keys[32'hFFFF_FFF0] = 1;
    unsigned_right = $right(unsigned_keys);
    unsigned_high = $high(unsigned_keys);
  end

  final begin
    if ($bits($low(wide)) != 64 || $bits($size(wide)) != 32)
      $fatal(1, "$low and $size of a longint index were %0d and %0d bits",
             $bits($low(wide)), $bits($size(wide)));
    if (wide_low !== -64'sh2_0000_0000)
      $fatal(1, "$low of a longint index was %0d", wide_low);
    if (wide_low_empty !== 0)
      $fatal(1, "$low of a longint index holding none was %0d", wide_low_empty);
    if (wide_size !== 2)
      $fatal(1, "$size was %0d, expected 2", wide_size);
    if (wide_increment !== -1)
      $fatal(1, "$increment was %0d, expected -1", wide_increment);
    if (chosen_high !== 64'sh1_0000_0000)
      $fatal(1, "$high of a computed dimension 1 was %0d", chosen_high);
    if (unsigned_right !== 32'hFFFF_FFFF)
      $fatal(1, "$right of an unsigned 32-bit index was %h", unsigned_right);
    if (unsigned_high !== 32'hFFFF_FFF0)
      $fatal(1, "$high of an unsigned 32-bit index was %h", unsigned_high);
    $display("All checks passed");
  end
endmodule

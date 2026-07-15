// LRM 20.7 array query functions over fixed-size dimensions, and the dimension
// counts. Each result is a property of the operand's type, so the operand is
// never evaluated and every call is an elaboration-time constant. Dimension 1
// is the slowest varying one; the optional second argument names a deeper one.
// `$increment` is 1 for a descending dimension and -1 for an ascending one, and
// `$low` / `$high` follow it, so a descending unpacked range reports the same
// shape a packed one does.
module Top;
  typedef logic [16:1] Word;

  Word             ram      [0:9];
  logic [3:0][7:0] packed2d;
  int              desc     [7:2];
  int              plain;
  bit              flag;
  string           s;

  int ram_dims, ram_updims;
  int ram_left, ram_right, ram_low, ram_high, ram_incr, ram_size;
  int word_size, word_left, word_right, word_low, word_high, word_incr;

  int p2d_dims, p2d_updims, p2d_size, p2d_inner_size, p2d_left, p2d_incr;

  int desc_size, desc_left, desc_right, desc_low, desc_high, desc_incr;
  int desc_dims, desc_updims;

  int plain_size, plain_left, plain_dims;
  int flag_dims;
  int s_dims, s_updims;

  // LRM 20.7 does not require the dimension to be a constant expression. Which
  // dimensions the operand has is still a property of its type, so the query
  // selects among their results by the index; an index the type has no
  // dimension for reads `'x`.
  int d;
  integer rt_size, rt_left, rt_incr, rt_out_of_range;

  initial begin
    ram_dims = $dimensions(ram);
    ram_updims = $unpacked_dimensions(ram);
    ram_left = $left(ram);
    ram_right = $right(ram);
    ram_low = $low(ram);
    ram_high = $high(ram);
    ram_incr = $increment(ram);
    ram_size = $size(ram);

    word_size = $size(ram, 2);
    word_left = $left(ram, 2);
    word_right = $right(ram, 2);
    word_low = $low(ram, 2);
    word_high = $high(ram, 2);
    word_incr = $increment(ram, 2);

    p2d_dims = $dimensions(packed2d);
    p2d_updims = $unpacked_dimensions(packed2d);
    p2d_size = $size(packed2d);
    p2d_inner_size = $size(packed2d, 2);
    p2d_left = $left(packed2d);
    p2d_incr = $increment(packed2d);

    desc_size = $size(desc);
    desc_left = $left(desc);
    desc_right = $right(desc);
    desc_low = $low(desc);
    desc_high = $high(desc);
    desc_incr = $increment(desc);
    desc_dims = $dimensions(desc);
    desc_updims = $unpacked_dimensions(desc);

    // An integral type of predefined width is one packed dimension of that
    // width; a scalar has no dimension at all.
    plain_size = $size(plain);
    plain_left = $left(plain);
    plain_dims = $dimensions(plain);
    flag_dims = $dimensions(flag);

    // A string is one dimension, and it is a packed one.
    s_dims = $dimensions(s);
    s_updims = $unpacked_dimensions(s);

    d = 2;
    rt_size = $size(ram, d);
    rt_left = $left(ram, d);
    rt_incr = $increment(ram, d);
    d = 5;
    rt_out_of_range = $size(ram, d);
  end
endmodule

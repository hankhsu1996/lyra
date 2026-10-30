// A union can be written through one member type and read through another
// (LRM 7.3.2's "potential type loophole"). Outside a common initial sequence
// the standard does not say which value such a read answers, but it is a read
// of the member and answers a value of the member's type: no wider than that
// type, the same value however often it is read, and equal to itself once
// copied into a variable of that type. Here every member is narrower than the
// one written, so the bits it is read from lie under bits the write set, an
// element of a member included. A tagged union held as a member is read the
// same way: whatever member its tag names, if any, answers a value of that
// member's type, and a copy of it can be made whatever its tag says. Its tag
// may name neither member, so a read through it is checked only where one
// happens.
module Top;
  typedef union {
    int word;
    bit [4:0] five;
    bit [12:0] thirteen;
    bit [4:0] five_in_array[2];
  } u_t;

  u_t u;

  typedef union tagged {
    bit [3:0] a;
    bit [3:0] b;
  } t_t;

  typedef union {
    bit [255:0] wide;
    bit [7:0] low;
    t_t t;
  } holder_t;

  holder_t h;
  t_t t_copy;

  int five_widened;
  int thirteen_widened;
  int five_ones;
  bit [4:0] five_copy;
  bit [12:0] thirteen_copy;
  bit five_equal_to_its_copy;
  bit thirteen_equal_to_its_copy;
  bit five_read_twice_equal;
  bit five_equal_in_a_wide_context;
  int element_ones;
  bit [4:0] element_copy;
  bit element_equal_to_its_copy;
  // -1 until a member of the held tagged union is read, which no value of a
  // 4-bit member is.
  int under_low_widened = -1;
  int under_wide_widened = -1;

  initial begin
    five_widened = -1;
    thirteen_widened = -1;
    five_ones = 99;
    u.word = -1;
    five_widened = u.five;
    thirteen_widened = u.thirteen;
    five_ones = $countones(u.five);
    five_copy = u.five;
    thirteen_copy = u.thirteen;
    five_equal_to_its_copy = (u.five == five_copy);
    thirteen_equal_to_its_copy = (u.thirteen == thirteen_copy);
    five_read_twice_equal = (u.five == u.five);
    five_equal_in_a_wide_context = (32'(u.five) == {27'b0, five_copy});
    element_ones = 99;
    element_ones = $countones(u.five_in_array[0]);
    element_copy = u.five_in_array[0];
    element_equal_to_its_copy = (u.five_in_array[0] == element_copy);

    h.t = tagged b 4'h5;
    h.low = 8'hF5;
    if (h.t matches tagged a .held) under_low_widened = held;
    if (h.t matches tagged b .held) under_low_widened = held;
    h.wide = '1;
    t_copy = h.t;
    if (t_copy matches tagged a .held) under_wide_widened = held;
    if (t_copy matches tagged b .held) under_wide_widened = held;
  end

  final begin
    if (five_widened < 0 || five_widened > 31)
      $fatal(1, "a 5-bit member read as %0d, outside its type", five_widened);
    if (thirteen_widened < 0 || thirteen_widened > 8191)
      $fatal(1, "a 13-bit member read as %0d, outside its type",
             thirteen_widened);
    if (five_ones > 5)
      $fatal(1, "a 5-bit member read with %0d bits set", five_ones);
    if (five_equal_to_its_copy !== 1'b1)
      $fatal(1, "a 5-bit member compared unequal to a copy of itself");
    if (thirteen_equal_to_its_copy !== 1'b1)
      $fatal(1, "a 13-bit member compared unequal to a copy of itself");
    if (five_read_twice_equal !== 1'b1)
      $fatal(1, "a 5-bit member read twice compared unequal");
    if (five_equal_in_a_wide_context !== 1'b1)
      $fatal(1, "a 5-bit member widened differently from a copy of itself");
    if (element_ones > 5)
      $fatal(1, "a 5-bit element of a member read with %0d bits set",
             element_ones);
    if (element_equal_to_its_copy !== 1'b1)
      $fatal(1, "a 5-bit element of a member compared unequal to its copy");
    if (under_low_widened !== -1 &&
        (under_low_widened < 0 || under_low_widened > 15))
      $fatal(1, "a tagged member's 4-bit member read as %0d, outside its type",
             under_low_widened);
    if (under_wide_widened !== -1 &&
        (under_wide_widened < 0 || under_wide_widened > 15))
      $fatal(1, "a copied tagged member's 4-bit member read as %0d, outside its type",
             under_wide_widened);
    $display("All checks passed");
  end
endmodule

// Each SystemVerilog type a DPI-C subroutine may name has one C counterpart,
// and the small ones -- the integer atoms, the reals, a scalar bit or logic, a
// chandle, and a string -- cross in a register rather than through a pointer,
// which is also the set a function result is restricted to (LRM 35.5.5,
// 35.5.6, Table H.1, Annex H.8.7, H.8.9). A `realtime` is a `real` (LRM
// 6.12) and crosses as C's `double`, while a `shortreal` is C's `float`, so it
// crosses at single precision and not as a `real` would. An enumeration
// crosses as its base type does (LRM 35.5.6, Annex H.7.3). Every answer below
// is derived from every argument of its call, and the arguments of a call
// differ from one another, so a value that failed to cross cannot produce it.
module Top;
  typedef enum int {
    kRed = 3,
    kGreen = 40,
    kBlue = 500
  } colour_t;

  import "DPI-C" function byte byte_mix(input byte a, input byte b);
  import "DPI-C" function shortint shortint_mix(
      input shortint a, input shortint b);
  import "DPI-C" function int int_mix(input int a, input int b);
  import "DPI-C" function longint longint_mix(
      input longint a, input longint b);
  import "DPI-C" function real real_mix(input real a, input real b);
  import "DPI-C" function realtime realtime_half(input realtime t);
  import "DPI-C" function shortreal shortreal_mix(
      input shortreal a, input shortreal b);
  import "DPI-C" function void shortreal_scale(
      inout shortreal value, output shortreal tripled);
  import "DPI-C" function void next_colour(
      input colour_t c, output colour_t previous, output colour_t next);
  import "DPI-C" function bit bit_xor(input bit a, input bit b);
  import "DPI-C" function logic rotate_logic(input logic v);
  import "DPI-C" function string join_text(input string a, input string b);
  import "DPI-C" function chandle make_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle box);
  import "DPI-C" function void free_cell(input chandle box);

  byte byte_answer;
  shortint shortint_answer;
  int int_answer;
  longint longint_answer;
  real real_answer;
  realtime halved_time;
  shortreal shortreal_answer;
  shortreal scaled;
  shortreal tripled;
  colour_t after_green;
  colour_t before_green;
  bit differing;
  bit matching;
  logic from_zero;
  logic from_one;
  logic from_z;
  logic from_x;
  string joined;
  chandle box;
  bit box_is_null;
  int box_value;

  initial begin
    byte_answer = byte_mix(8'sd10, 8'sd7);
    shortint_answer = shortint_mix(16'sd200, 16'sd45);
    int_answer = int_mix(12345, 678);
    longint_answer = longint_mix(64'sd9, 64'sd123456789);
    real_answer = real_mix(2.5, 0.125);
    halved_time = realtime_half(12.5);

    // The foreign side rounds to single precision after each step, which for
    // these operands lands one unit away from their product taken in `double`
    // and rounded once, so the answer is compared by its bits.
    shortreal_answer = shortreal_mix(0.3, 7.0);

    scaled = 1.5;
    tripled = 0.0;
    shortreal_scale(scaled, tripled);

    before_green = kBlue;
    after_green = kRed;
    next_colour(kGreen, before_green, after_green);

    differing = bit_xor(1'b1, 1'b0);
    matching = bit_xor(1'b1, 1'b1);

    // The rotation is one-to-one over the four values a scalar logic can
    // carry, so an argument that never crossed changes the answer.
    from_zero = rotate_logic(1'b0);
    from_one = rotate_logic(1'b1);
    from_z = rotate_logic(1'bz);
    from_x = rotate_logic(1'bx);

    joined = join_text("ab", "cd");

    box = make_cell(6);
    box_is_null = (box == null);
    box_value = read_cell(box);
    free_cell(box);
  end

  final begin
    if (byte_answer !== 8'sd37)
      $fatal(1, "byte_answer was %0d, expected 37", byte_answer);
    if (shortint_answer !== 16'sd20045)
      $fatal(1, "shortint_answer was %0d, expected 20045", shortint_answer);
    if (int_answer !== 12345678)
      $fatal(1, "int_answer was %0d, expected 12345678", int_answer);
    if (longint_answer !== 64'sd9123456789)
      $fatal(1, "longint_answer was %0d, expected 9123456789", longint_answer);
    if (real_answer != 20.125)
      $fatal(1, "real_answer was %f, expected 20.125", real_answer);
    if (halved_time != 6.25)
      $fatal(1, "halved_time was %f, expected 6.25", halved_time);
    if ($shortrealtobits(shortreal_answer) !== 32'h3F21_47AE)
      $fatal(1, "shortreal_answer had bits %h, expected 3f2147ae",
             $shortrealtobits(shortreal_answer));
    if (scaled != 3.0) $fatal(1, "scaled was %f, expected 3.0", scaled);
    if (tripled != 4.5) $fatal(1, "tripled was %f, expected 4.5", tripled);
    if (after_green !== kBlue)
      $fatal(1, "after_green was %0d, expected 500", after_green);
    if (before_green !== kRed)
      $fatal(1, "before_green was %0d, expected 3", before_green);
    if (differing !== 1'b1)
      $fatal(1, "differing was %b, expected 1", differing);
    if (matching !== 1'b0)
      $fatal(1, "matching was %b, expected 0", matching);
    if (from_zero !== 1'b1)
      $fatal(1, "rotating 0 gave %b, expected 1", from_zero);
    if (from_one !== 1'bz)
      $fatal(1, "rotating 1 gave %b, expected z", from_one);
    if (from_z !== 1'bx) $fatal(1, "rotating z gave %b, expected x", from_z);
    if (from_x !== 1'b0) $fatal(1, "rotating x gave %b, expected 0", from_x);
    if (joined != "ab|cd")
      $fatal(1, "joined was '%s', expected 'ab|cd'", joined);
    if (box_is_null !== 1'b0) $fatal(1, "make_cell returned a null chandle");
    if (box_value !== 43)
      $fatal(1, "box_value was %0d, expected 43", box_value);
    $display("All checks passed");
  end
endmodule

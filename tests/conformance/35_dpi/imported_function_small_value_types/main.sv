// Each SystemVerilog type a DPI-C subroutine may name has one C counterpart,
// and the small ones -- the integer atoms, the reals, a scalar bit or logic, a
// chandle, and a string -- cross in a register rather than through a pointer,
// which is also the set a function result is restricted to (LRM 35.5.5,
// 35.5.6, Table H.1, Annex H.8.7, H.8.9). Every answer below is derived from
// every argument of its call, and the arguments of a call differ from one
// another, so a value that failed to cross cannot produce it.
module Top;
  import "DPI-C" function byte byte_mix(input byte a, input byte b);
  import "DPI-C" function shortint shortint_mix(
      input shortint a, input shortint b);
  import "DPI-C" function int int_mix(input int a, input int b);
  import "DPI-C" function longint longint_mix(
      input longint a, input longint b);
  import "DPI-C" function real real_mix(input real a, input real b);
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

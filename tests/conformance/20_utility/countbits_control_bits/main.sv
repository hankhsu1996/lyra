// $countbits returns the number of bits of a bit-stream operand whose value
// matches one of the control bits it is given. A control bit wider than one
// bit contributes only its least significant bit, and a value named more than
// once counts as though it had been named once. $countones, $onehot and
// $onehot0 are defined in terms of it, counting only bits whose value is 1, so
// an x bit is not a one (LRM 20.9).
module Top;
  typedef struct {
    int p;
    logic [3:0] q;
  } pair_t;

  logic [7:0] mixed;
  bit [7:0] known;
  logic [99:0] wide;
  pair_t pair;
  logic [3:0] words[2];
  string text;

  int ones;
  int zeros;
  int unknown;
  int high_impedance;
  int repeated_control;
  int every_control;
  int wide_control;

  int two_state_unknown;
  int two_state_ones;
  int two_state_zeros;

  int wide_ones;
  int wide_zeros;

  int struct_ones;
  int array_ones;
  int text_ones;
  int text_zeros;

  bit onehot_single;
  bit onehot_none;
  bit onehot_many;
  bit onehot_with_unknown;
  bit onehot0_none;
  bit onehot0_many;

  initial begin
    mixed = 8'b1010_01xz;
    ones = $countbits(mixed, 1'b1);
    zeros = $countbits(mixed, 1'b0);
    unknown = $countbits(mixed, 1'bx);
    high_impedance = $countbits(mixed, 1'bz);
    repeated_control = $countbits(mixed, 1'b1, 1'b1);
    every_control = $countbits(mixed, 1'b0, 1'b1, 1'bx, 1'bz);
    wide_control = $countbits(mixed, 2'b01);

    known = 8'b1010_0100;
    two_state_unknown = $countbits(known, 1'bx);
    two_state_ones = $countones(known);
    two_state_zeros = $countbits(known, 1'b0);

    wide = 100'b0;
    wide[99] = 1'b1;
    wide[64] = 1'b1;
    wide[63] = 1'b1;
    wide[0] = 1'b1;
    wide_ones = $countones(wide);
    wide_zeros = $countbits(wide, 1'b0);

    pair.p = 3;
    pair.q = 4'b1001;
    struct_ones = $countones(pair);

    words[0] = 4'b1100;
    words[1] = 4'b0001;
    array_ones = $countones(words);

    text = "A";
    text_ones = $countones(text);
    text_zeros = $countbits(text, 1'b0);

    onehot_single = $onehot(8'b0010_0000);
    onehot_none = $onehot(8'b0000_0000);
    onehot_many = $onehot(8'b1010_0000);
    onehot_with_unknown = $onehot(8'b0000_001x);
    onehot0_none = $onehot0(8'b0000_0000);
    onehot0_many = $onehot0(8'b1010_0000);
  end

  final begin
    if (ones !== 3) $fatal(1, "ones was %0d, expected 3", ones);
    if (zeros !== 3) $fatal(1, "zeros was %0d, expected 3", zeros);
    if (unknown !== 1) $fatal(1, "unknown was %0d, expected 1", unknown);
    if (high_impedance !== 1)
      $fatal(1, "high_impedance was %0d, expected 1", high_impedance);
    if (repeated_control !== 3)
      $fatal(1, "repeated_control was %0d, expected 3", repeated_control);
    if (every_control !== 8)
      $fatal(1, "every_control was %0d, expected 8", every_control);
    if (wide_control !== 3)
      $fatal(1, "wide_control was %0d, expected 3", wide_control);

    if (two_state_unknown !== 0)
      $fatal(1, "two_state_unknown was %0d, expected 0", two_state_unknown);
    if (two_state_ones !== 3)
      $fatal(1, "two_state_ones was %0d, expected 3", two_state_ones);
    if (two_state_zeros !== 5)
      $fatal(1, "two_state_zeros was %0d, expected 5", two_state_zeros);

    if (wide_ones !== 4) $fatal(1, "wide_ones was %0d, expected 4", wide_ones);
    if (wide_zeros !== 96)
      $fatal(1, "wide_zeros was %0d, expected 96", wide_zeros);

    if (struct_ones !== 4)
      $fatal(1, "struct_ones was %0d, expected 4", struct_ones);
    if (array_ones !== 3)
      $fatal(1, "array_ones was %0d, expected 3", array_ones);
    if (text_ones !== 2)
      $fatal(1, "text_ones was %0d, expected 2", text_ones);
    if (text_zeros !== 6)
      $fatal(1, "text_zeros was %0d, expected 6", text_zeros);

    if (onehot_single !== 1'b1)
      $fatal(1, "onehot_single was %b, expected 1", onehot_single);
    if (onehot_none !== 1'b0)
      $fatal(1, "onehot_none was %b, expected 0", onehot_none);
    if (onehot_many !== 1'b0)
      $fatal(1, "onehot_many was %b, expected 0", onehot_many);
    if (onehot_with_unknown !== 1'b1)
      $fatal(1, "onehot_with_unknown was %b, expected 1", onehot_with_unknown);
    if (onehot0_none !== 1'b1)
      $fatal(1, "onehot0_none was %b, expected 1", onehot0_none);
    if (onehot0_many !== 1'b0)
      $fatal(1, "onehot0_many was %b, expected 0", onehot0_many);
    $display("All checks passed");
  end
endmodule

// $realtobits exposes a real as the 64-bit vector its IEEE 754 double
// representation spells, and $bitstoreal reads such a pattern back as a real,
// so a value carried out and back arrives unchanged however many mantissa bits
// it uses. The shortreal pair does the same over a 32-bit single-precision
// pattern (LRM 20.5).
module Top;
  real one = 1.0;
  real two = 2.0;
  real minus_one = -1.0;
  real three = 3.0;
  shortreal short_one = 1.0;
  shortreal short_minus_two = -2.0;

  bit [63:0] bits_one = '1;
  bit [63:0] bits_two = '1;
  bit [63:0] bits_minus_one = '1;
  bit [31:0] short_bits_one = '1;
  bit [31:0] short_bits_minus_two = '1;

  bit [63:0] pattern_for_two;
  bit [31:0] short_pattern_for_one;

  real from_pattern = -1.0;
  real round_tripped = -1.0;
  real messy = -1.0;
  shortreal short_from_pattern = -1.0;

  int double_width = -1;
  int single_width = -1;

  initial begin
    messy = one / three;

    bits_one = $realtobits(one);
    bits_two = $realtobits(two);
    bits_minus_one = $realtobits(minus_one);
    short_bits_one = $shortrealtobits(short_one);
    short_bits_minus_two = $shortrealtobits(short_minus_two);

    pattern_for_two = 64'h4000_0000_0000_0000;
    short_pattern_for_one = 32'h3F80_0000;
    from_pattern = $bitstoreal(pattern_for_two);
    round_tripped = $bitstoreal($realtobits(messy));
    short_from_pattern = $bitstoshortreal(short_pattern_for_one);

    double_width = $bits($realtobits(one));
    single_width = $bits($shortrealtobits(short_one));
  end

  final begin
    if (bits_one !== 64'h3FF0_0000_0000_0000)
      $fatal(1, "$realtobits(1.0) was %h, expected 3ff0000000000000", bits_one);
    if (bits_two !== 64'h4000_0000_0000_0000)
      $fatal(1, "$realtobits(2.0) was %h, expected 4000000000000000", bits_two);
    if (bits_minus_one !== 64'hBFF0_0000_0000_0000)
      $fatal(1, "$realtobits(-1.0) was %h, expected bff0000000000000",
             bits_minus_one);
    if (short_bits_one !== 32'h3F80_0000)
      $fatal(1, "$shortrealtobits(1.0) was %h, expected 3f800000",
             short_bits_one);
    if (short_bits_minus_two !== 32'hC000_0000)
      $fatal(1, "$shortrealtobits(-2.0) was %h, expected c0000000",
             short_bits_minus_two);

    if (from_pattern != 2.0)
      $fatal(1, "$bitstoreal of the pattern for 2.0 was %g", from_pattern);
    if (round_tripped != messy)
      $fatal(1, "a real carried through its bit pattern came back as %.17g",
             round_tripped);
    if (short_from_pattern != 1.0)
      $fatal(1, "$bitstoshortreal of the pattern for 1.0 was %g",
             short_from_pattern);

    if (double_width !== 64)
      $fatal(1, "$realtobits yielded %0d bits, expected 64", double_width);
    if (single_width !== 32)
      $fatal(1, "$shortrealtobits yielded %0d bits, expected 32", single_width);

    $display("All checks passed");
  end
endmodule

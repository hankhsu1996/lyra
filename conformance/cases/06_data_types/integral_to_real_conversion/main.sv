// Implicit conversion takes place when an integral expression is assigned to
// a real, and the value converted is the one the source denotes: a signed
// source retains the significance of its sign and an unsigned one does not,
// so the same bits reach the real as a negative or a positive number, and a
// source of any width reaches it whole -- wider than 32 bits, an unsigned
// 64-bit value with its top bit set, wider than 64 bits and negative --
// whether by assignment, by a cast or by `$itor` (LRM 6.24.1, 20.5). Where no
// real is the number, it lands on one of the two nearest it: a real is the
// same as a C double (LRM 6.12), and C converts such an integer to either
// neighbour (ISO C 6.3.1.4). Individual bits that are x or z are treated as
// zero upon conversion, so a partly unknown source converts to the value its
// remaining bits denote rather than to an unknown (LRM 6.12.1, 6.8, 6.11.3).
module Top;
  real from_int;
  real from_negative_int;
  real from_byte;
  real from_negative_byte;
  real from_unsigned_vector;
  real from_signed_vector;
  real from_longint;
  real from_literal;
  real from_unknown_bits;
  real from_high_impedance_bits;
  shortreal from_int_to_shortreal;
  real from_unsigned_top_bit;
  real from_wide;
  real from_wide_negative;
  real from_wide_rounded;
  real from_wide_cast;
  real from_wide_itor;
  real from_wide_unknown;
  shortreal from_wide_to_shortreal;

  initial begin
    int whole;
    byte signed_byte;
    bit [7:0] unsigned_bits;
    bit signed [7:0] signed_bits;
    longint wide;
    logic [7:0] partly_unknown;
    logic [7:0] partly_high_impedance;
    bit [63:0] all_ones;
    logic [99:0] above_64;
    logic signed [99:0] negative_wide;
    logic [128:0] needs_rounding;
    logic [99:0] wide_unknown;

    whole = 42;
    from_int = whole;
    from_int_to_shortreal = whole;

    whole = -42;
    from_negative_int = whole;

    signed_byte = 50;
    from_byte = signed_byte;
    signed_byte = -7;
    from_negative_byte = signed_byte;

    // The same eight set bits read once as an unsigned value and once as a
    // signed one, so only the signedness rule separates the two results.
    unsigned_bits = 8'hFF;
    from_unsigned_vector = unsigned_bits;
    signed_bits = 8'hFF;
    from_signed_vector = signed_bits;

    // A value that does not fit in 32 bits, so a source truncated on the way
    // to the real is told apart from one carried whole.
    wide = 64'd4294967296;
    from_longint = wide;

    from_literal = 100;

    partly_unknown = 8'b1xxx_0001;
    from_unknown_bits = partly_unknown;
    partly_high_impedance = 8'b1zzz_0001;
    from_high_impedance_bits = partly_high_impedance;

    all_ones = '1;
    from_unsigned_top_bit = all_ones;
    above_64 = 100'd1 << 64;
    from_wide = above_64;
    from_wide_cast = real'(above_64);
    from_wide_itor = $itor(above_64);
    from_wide_to_shortreal = above_64;
    negative_wide = -100'sd3;
    from_wide_negative = negative_wide;
    // Between 2**128 and 2**128 + 2**76, so a source carried only in part
    // lands on neither.
    needs_rounding = (129'd1 << 128) | (129'd1 << 75) | 129'd1;
    from_wide_rounded = needs_rounding;
    wide_unknown = (100'd1 << 70) | 100'bx;
    from_wide_unknown = wide_unknown;
  end

  final begin
    if (from_int != 42.0)
      $fatal(1, "from_int was %g, expected 42.0", from_int);
    if (from_negative_int != -42.0)
      $fatal(1, "from_negative_int was %g, expected -42.0", from_negative_int);
    if (from_byte != 50.0)
      $fatal(1, "from_byte was %g, expected 50.0", from_byte);
    if (from_negative_byte != -7.0)
      $fatal(1, "from_negative_byte was %g, expected -7.0",
             from_negative_byte);
    if (from_unsigned_vector != 255.0)
      $fatal(1, "from_unsigned_vector was %g, expected 255.0",
             from_unsigned_vector);
    if (from_signed_vector != -1.0)
      $fatal(1, "from_signed_vector was %g, expected -1.0",
             from_signed_vector);
    if (from_longint != 4294967296.0)
      $fatal(1, "from_longint was %g, expected 4294967296.0", from_longint);
    if (from_literal != 100.0)
      $fatal(1, "from_literal was %g, expected 100.0", from_literal);
    if (from_unknown_bits != 129.0)
      $fatal(1, "from_unknown_bits was %g, expected 129.0", from_unknown_bits);
    if (from_high_impedance_bits != 129.0)
      $fatal(1, "from_high_impedance_bits was %g, expected 129.0",
             from_high_impedance_bits);
    if (from_int_to_shortreal != 42.0)
      $fatal(1, "from_int_to_shortreal was %g, expected 42.0",
             from_int_to_shortreal);
    if (from_unsigned_top_bit != 2.0 ** 64 &&
        from_unsigned_top_bit != 2.0 ** 64 - 2048.0)
      $fatal(1, "from_unsigned_top_bit was %g, a real neighbouring 2**64 - 1",
             from_unsigned_top_bit);
    if (from_wide != 2.0 ** 64)
      $fatal(1, "from_wide was %g, expected 2**64", from_wide);
    if (from_wide_cast != 2.0 ** 64)
      $fatal(1, "from_wide_cast was %g, expected 2**64", from_wide_cast);
    if (from_wide_itor != 2.0 ** 64)
      $fatal(1, "from_wide_itor was %g, expected 2**64", from_wide_itor);
    if (from_wide_to_shortreal != 2.0 ** 64)
      $fatal(1, "from_wide_to_shortreal was %g, expected 2**64",
             from_wide_to_shortreal);
    if (from_wide_negative != -3.0)
      $fatal(1, "from_wide_negative was %g, expected -3.0",
             from_wide_negative);
    if (from_wide_rounded != 2.0 ** 128 + 2.0 ** 76 &&
        from_wide_rounded != 2.0 ** 128)
      $fatal(1, "from_wide_rounded was %g, a real neighbouring its number",
             from_wide_rounded);
    if (from_wide_unknown != 2.0 ** 70)
      $fatal(1, "from_wide_unknown was %g, expected 2**70",
             from_wide_unknown);
    $display("All checks passed");
  end
endmodule

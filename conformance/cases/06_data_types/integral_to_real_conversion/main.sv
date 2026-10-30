// Implicit conversion takes place when an integral expression is assigned to
// a real, and the value converted is the one the source denotes: a signed
// source retains the significance of its sign and an unsigned one does not,
// so the same bits reach the real as a negative or a positive number, and a
// source wider than 32 bits reaches it whole. Individual bits that are x or z
// are treated as zero upon conversion, so a partly unknown source converts to
// the value its remaining bits denote rather than to an unknown
// (LRM 6.12.1, 6.8, 6.11.3).
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

  initial begin
    int whole;
    byte signed_byte;
    bit [7:0] unsigned_bits;
    bit signed [7:0] signed_bits;
    longint wide;
    logic [7:0] partly_unknown;
    logic [7:0] partly_high_impedance;

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
    $display("All checks passed");
  end
endmodule

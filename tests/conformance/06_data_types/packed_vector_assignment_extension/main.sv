// When the right-hand side of an assignment is narrower than the left-hand
// side, the value is padded to the left-hand width: an unsigned value is
// zero-extended and a signed value is sign-extended. Sign extension
// replicates the sign bit whatever it holds, so a sign bit of x or z fills
// the added positions with x or z. An assignment between equal widths copies
// every bit unchanged. The rule is stated over the two declared widths and
// nothing else, so it reads the same at a width no narrow example reaches
// (LRM 6.11.2, 6.11.3, 10.7).
module Top;
  bit [7:0] signed_source;
  logic [7:0] unsigned_source;
  logic [7:0] sign_bit_unknown;
  logic [7:0] sign_bit_high_impedance;
  logic [7:0] unsigned_keeps_unknown;
  bit [3:0] equal_width_two_state;
  logic [3:0] equal_width_four_state;
  bit [95:0] wide_signed_source;
  bit [64:0] wide_unsigned_source;
  logic [127:0] wide_sign_bit_unknown;

  initial begin
    bit signed [3:0] narrow_signed;
    bit [3:0] narrow_unsigned;
    logic signed [3:0] narrow_signed_four_state;
    logic [3:0] narrow_four_state;
    bit signed [32:0] wide_narrow_signed;
    bit [63:0] wide_narrow_unsigned;
    logic signed [64:0] wide_narrow_signed_four_state;

    // The same bit pattern in the low four bits, so only the padding rule
    // separates the two results.
    narrow_signed = 4'sb1010;
    signed_source = narrow_signed;
    narrow_unsigned = 4'b1010;
    unsigned_source = narrow_unsigned;

    narrow_signed_four_state = 4'bx010;
    sign_bit_unknown = narrow_signed_four_state;
    narrow_signed_four_state = 4'bz010;
    sign_bit_high_impedance = narrow_signed_four_state;

    narrow_four_state = 4'b10xz;
    unsigned_keeps_unknown = narrow_four_state;

    equal_width_two_state = narrow_unsigned;
    equal_width_four_state = narrow_four_state;

    // The padding a signed source asks for here is wider than the source, and
    // the padding an unsigned one asks for begins exactly where the source
    // ends: two positions the rule covers and a small example never states.
    wide_narrow_signed = 33'h1_0000_0001;
    wide_signed_source = wide_narrow_signed;

    wide_narrow_unsigned = 64'hFFFF_FFFF_FFFF_FFFF;
    wide_unsigned_source = wide_narrow_unsigned;

    wide_narrow_signed_four_state = {1'bx, 64'h0000_0000_0000_00A5};
    wide_sign_bit_unknown = wide_narrow_signed_four_state;
  end

  final begin
    if (signed_source !== 8'b11111010)
      $fatal(1, "signed_source was %b, expected 11111010", signed_source);
    if (unsigned_source !== 8'b00001010)
      $fatal(1, "unsigned_source was %b, expected 00001010", unsigned_source);
    if (sign_bit_unknown !== 8'bxxxxx010)
      $fatal(1, "sign_bit_unknown was %b, expected xxxxx010",
             sign_bit_unknown);
    if (sign_bit_high_impedance !== 8'bzzzzz010)
      $fatal(1, "sign_bit_high_impedance was %b, expected zzzzz010",
             sign_bit_high_impedance);
    if (unsigned_keeps_unknown !== 8'b000010xz)
      $fatal(1, "unsigned_keeps_unknown was %b, expected 000010xz",
             unsigned_keeps_unknown);
    if (equal_width_two_state !== 4'b1010)
      $fatal(1, "equal_width_two_state was %b, expected 1010",
             equal_width_two_state);
    if (equal_width_four_state !== 4'b10xz)
      $fatal(1, "equal_width_four_state was %b, expected 10xz",
             equal_width_four_state);
    if (wide_signed_source !== 96'hFFFF_FFFF_FFFF_FFFF_0000_0001)
      $fatal(1, "wide_signed_source was %h, expected ffffffffffffffff00000001",
             wide_signed_source);
    if (wide_unsigned_source !== 65'h0_FFFF_FFFF_FFFF_FFFF)
      $fatal(1, "wide_unsigned_source was %h, expected 0ffffffffffffffff",
             wide_unsigned_source);
    if (wide_sign_bit_unknown !== {{64{1'bx}}, 64'h0000_0000_0000_00A5})
      $fatal(1, "wide_sign_bit_unknown was %h, expected sixteen x digits then 00000000000000a5",
             wide_sign_bit_unknown);
    $display("All checks passed");
  end
endmodule

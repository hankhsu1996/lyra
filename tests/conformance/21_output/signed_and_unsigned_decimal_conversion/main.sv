// A decimal conversion renders the value of its operand, so one bit pattern
// prints as a negative number through an operand declared signed and as its
// magnitude through an unsigned one of the same width. The operand's width
// bounds the value but not the machinery: a value too wide to hold in a
// machine integer converts to all of its digits. A field width of zero asks
// for the minimum width, so no padding stands between the value and the text
// (LRM 21.2.1.1, 21.2.1.2).
module Top;
  bit [7:0] small_unsigned;
  bit signed [7:0] small_signed;
  bit [7:0] same_bits_unsigned;
  bit [127:0] wide_unsigned;
  bit signed [127:0] wide_signed;

  string small_unsigned_text;
  string small_signed_text;
  string same_bits_unsigned_text;
  string wide_unsigned_text;
  string wide_signed_text;

  initial begin
    small_unsigned = 8'd42;
    small_signed = 8'shFB;
    same_bits_unsigned = 8'hFB;
    wide_unsigned = 128'd12345678901234567890;
    wide_signed = 128'shFFFF_FFFF_FFFF_FFFF_54AB_5673_14E0_F52E;

    small_unsigned_text = $sformatf("%0d", small_unsigned);
    small_signed_text = $sformatf("%0d", small_signed);
    same_bits_unsigned_text = $sformatf("%0d", same_bits_unsigned);
    wide_unsigned_text = $sformatf("%0d", wide_unsigned);
    wide_signed_text = $sformatf("%0d", wide_signed);
  end

  final begin
    if (small_unsigned_text != "42")
      $fatal(1, "decimal of 8'd42 was '%s', expected 42",
             small_unsigned_text);
    if (small_signed_text != "-5")
      $fatal(1, "decimal of a signed 8'hfb was '%s', expected -5",
             small_signed_text);
    if (same_bits_unsigned_text != "251")
      $fatal(1, "decimal of an unsigned 8'hfb was '%s', expected 251",
             same_bits_unsigned_text);
    if (wide_unsigned_text != "12345678901234567890")
      $fatal(1, "decimal of a 128-bit value was '%s', expected %s",
             wide_unsigned_text, "12345678901234567890");
    if (wide_signed_text != "-12345678901234567890")
      $fatal(1, "decimal of a signed 128-bit value was '%s', expected %s",
             wide_signed_text, "-12345678901234567890");
    $display("All checks passed");
  end
endmodule

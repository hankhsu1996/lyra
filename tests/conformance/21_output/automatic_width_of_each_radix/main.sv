// With no field width, a conversion is given the number of characters the
// largest value its operand could hold needs in that radix: one character per
// bit in binary, one per group of three bits in octal, one per group of four
// in hexadecimal, a leftover group counting as a whole character. In these
// radices the leading zeros of that field are printed rather than suppressed,
// and a field width of zero asks instead for the minimum width, which drops
// them. It is the operand's own width that fixes the count, so a value too
// wide to hold in a machine integer is converted whole
// (LRM 21.2.1.2, 21.2.1.3).
module Top;
  bit [7:0] byte_value;
  bit [15:0] hex_with_leading_zeros;
  bit [5:0] narrow_octal;
  bit [11:0] octal_with_leading_zeros;
  bit [7:0] binary_with_leading_zeros;
  bit [64:0] wide_hex;
  bit [64:0] wide_hex_with_leading_zeros;
  bit [127:0] wide_binary;

  string byte_hex;
  string sized_hex;
  string minimum_hex;
  string narrow_octal_text;
  string sized_octal;
  string minimum_octal;
  string sized_binary;
  string minimum_binary;
  string wide_hex_text;
  string wide_sized_hex;
  string wide_minimum_hex;
  string wide_binary_text;

  initial begin
    byte_value = 8'hAB;
    hex_with_leading_zeros = 16'h00AB;
    narrow_octal = 6'o42;
    octal_with_leading_zeros = 12'o0042;
    binary_with_leading_zeros = 8'b0000_1010;
    wide_hex = 65'h1_FFFF_FFFF_FFFF_FFFF;
    wide_hex_with_leading_zeros = 65'h0_0000_0000_0000_00AB;
    wide_binary = 128'h0123_4567_89AB_CDEF_0011_2233_4455_6677;

    byte_hex = $sformatf("%h", byte_value);
    sized_hex = $sformatf("%h", hex_with_leading_zeros);
    minimum_hex = $sformatf("%0h", hex_with_leading_zeros);
    narrow_octal_text = $sformatf("%o", narrow_octal);
    sized_octal = $sformatf("%o", octal_with_leading_zeros);
    minimum_octal = $sformatf("%0o", octal_with_leading_zeros);
    sized_binary = $sformatf("%b", binary_with_leading_zeros);
    minimum_binary = $sformatf("%0b", binary_with_leading_zeros);
    wide_hex_text = $sformatf("%h", wide_hex);
    wide_sized_hex = $sformatf("%h", wide_hex_with_leading_zeros);
    wide_minimum_hex = $sformatf("%0h", wide_hex_with_leading_zeros);
    wide_binary_text = $sformatf("%b", wide_binary);
  end

  final begin
    if (byte_hex != "ab")
      $fatal(1, "hex of an 8-bit value was '%s', expected ab", byte_hex);
    if (sized_hex != "00ab")
      $fatal(1, "hex of a 16-bit value was '%s', expected 00ab", sized_hex);
    if (minimum_hex != "ab")
      $fatal(1, "minimum-width hex was '%s', expected ab", minimum_hex);

    if (narrow_octal_text != "42")
      $fatal(1, "octal of a 6-bit value was '%s', expected 42",
             narrow_octal_text);
    if (sized_octal != "0042")
      $fatal(1, "octal of a 12-bit value was '%s', expected 0042",
             sized_octal);
    if (minimum_octal != "42")
      $fatal(1, "minimum-width octal was '%s', expected 42", minimum_octal);

    if (sized_binary != "00001010")
      $fatal(1, "binary of an 8-bit value was '%s', expected 00001010",
             sized_binary);
    if (minimum_binary != "1010")
      $fatal(1, "minimum-width binary was '%s', expected 1010",
             minimum_binary);

    if (wide_hex_text != "1ffffffffffffffff")
      $fatal(1, "hex of a 65-bit value was '%s', expected 17 digits",
             wide_hex_text);
    if (wide_sized_hex != "000000000000000ab")
      $fatal(1, "hex of a 65-bit value was '%s', expected 15 zeros then ab",
             wide_sized_hex);
    if (wide_minimum_hex != "ab")
      $fatal(1, "minimum-width hex of a 65-bit value was '%s', expected ab",
             wide_minimum_hex);

    if (wide_binary_text !=
        {"00000001001000110100010101100111",
         "10001001101010111100110111101111",
         "00000000000100010010001000110011",
         "01000100010101010110011001110111"})
      $fatal(1, "binary of a 128-bit value was '%s'", wide_binary_text);
    $display("All checks passed");
  end
endmodule

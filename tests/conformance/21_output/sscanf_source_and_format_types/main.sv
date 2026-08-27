// The source $sscanf reads and the control string either scan function takes
// may be an expression of integral, unpacked array of byte, or string type. An
// integral value is read as the bytes its bits spell, most significant byte
// first, and a byte array as the bytes it holds in order (LRM 21.3.4.3).
module Top;
  bit [31:0] two_state_source;
  logic [39:0] four_state_source;
  bit [39:0] two_state_format;
  logic [39:0] four_state_format;
  byte byte_source[0:10];

  int from_two_state_source;
  int digits;

  int from_four_state_source;
  int first_of_pair;
  int second_of_pair;

  int from_two_state_format;
  int format_first;
  int format_second;

  int from_four_state_format;
  int format_decimal;
  logic [15:0] format_hexadecimal;

  int from_byte_source;
  string byte_word;
  int byte_number;

  initial begin
    // "1234", "56 78", "%d %d", "%d %h", and "hello 42" followed by padding.
    two_state_source = 32'h31_32_33_34;
    four_state_source = 40'h35_36_20_37_38;
    two_state_format = 40'h25_64_20_25_64;
    four_state_format = 40'h25_64_20_25_68;
    byte_source = '{8'h68, 8'h65, 8'h6C, 8'h6C, 8'h6F,
                    8'h20,
                    8'h34, 8'h32,
                    8'h00, 8'h00, 8'h00};

    from_two_state_source = $sscanf(two_state_source, "%d", digits);
    from_four_state_source = $sscanf(four_state_source, "%d %d",
                                     first_of_pair, second_of_pair);
    from_two_state_format = $sscanf("10 20", two_state_format, format_first,
                                    format_second);
    from_four_state_format = $sscanf("99 cafe", four_state_format,
                                     format_decimal, format_hexadecimal);
    from_byte_source = $sscanf(byte_source, "%s %d", byte_word, byte_number);
  end

  final begin
    if (from_two_state_source !== 1)
      $fatal(1, "a two-state integral source returned %0d, expected 1",
             from_two_state_source);
    if (digits !== 1234)
      $fatal(1, "the value read from a packed source was %0d, expected 1234",
             digits);

    if (from_four_state_source !== 2)
      $fatal(1, "a four-state integral source returned %0d, expected 2",
             from_four_state_source);
    if (first_of_pair !== 56 || second_of_pair !== 78)
      $fatal(1, "the values read were %0d and %0d, expected 56 and 78",
             first_of_pair, second_of_pair);

    if (from_two_state_format !== 2)
      $fatal(1, "a two-state integral control string returned %0d",
             from_two_state_format);
    if (format_first !== 10 || format_second !== 20)
      $fatal(1, "the values read were %0d and %0d, expected 10 and 20",
             format_first, format_second);

    if (from_four_state_format !== 2)
      $fatal(1, "a four-state integral control string returned %0d",
             from_four_state_format);
    if (format_decimal !== 99)
      $fatal(1, "the decimal value was %0d, expected 99", format_decimal);
    if (format_hexadecimal !== 16'hcafe)
      $fatal(1, "the hexadecimal value was %h, expected cafe",
             format_hexadecimal);

    if (from_byte_source !== 2)
      $fatal(1, "a byte array source returned %0d, expected 2",
             from_byte_source);
    if (byte_word != "hello")
      $fatal(1, "the word read from a byte array was '%s', expected 'hello'",
             byte_word);
    if (byte_number !== 42)
      $fatal(1, "the number read from a byte array was %0d, expected 42",
             byte_number);
    $display("All checks passed");
  end
endmodule

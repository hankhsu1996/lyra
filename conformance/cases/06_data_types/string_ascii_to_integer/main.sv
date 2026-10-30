// atoi, atohex, atooct and atobin read a string as a number written in
// decimal, hexadecimal, octal and binary. Each scans the leading digits and
// underscore characters, stops at the first character that is neither or at
// the end of the string, and returns zero when it read no digits at all. They
// differ only in the base, so one text yields a different number to each of
// them. The result is 32 bits wide (LRM 6.16.9).
module Top;
  string digits = "123";
  string underscored = "12_34";
  string trailing_text = "42xyz";
  string letters = "abc";
  string blank = "";
  string upper_hex_text = "FF";
  string mixed_hex_text = "deadBEEF";
  string sevens = "77";
  string ones_and_zeros = "1010";

  integer plain = 999;
  integer with_underscores = 999;
  integer stops_at_letter = 999;
  integer no_digits = 999;
  integer from_blank = 999;

  integer upper_hex = 999;
  integer mixed_hex = 999;
  integer digits_as_hex = 999;
  integer digits_as_octal = 999;
  integer sevens_as_octal = 999;
  integer sevens_as_decimal = 999;
  integer ones_and_zeros_as_binary = 999;
  integer ones_and_zeros_as_decimal = 999;

  initial begin
    plain = digits.atoi();
    with_underscores = underscored.atoi();
    stops_at_letter = trailing_text.atoi();
    no_digits = letters.atoi();
    from_blank = blank.atoi();

    upper_hex = upper_hex_text.atohex();
    mixed_hex = mixed_hex_text.atohex();
    digits_as_hex = digits.atohex();
    digits_as_octal = digits.atooct();
    sevens_as_octal = sevens.atooct();
    sevens_as_decimal = sevens.atoi();
    ones_and_zeros_as_binary = ones_and_zeros.atobin();
    ones_and_zeros_as_decimal = ones_and_zeros.atoi();
  end

  final begin
    if (plain !== 123) $fatal(1, "plain was %0d, expected 123", plain);
    if (with_underscores !== 1234)
      $fatal(1, "with_underscores was %0d, expected 1234", with_underscores);
    if (stops_at_letter !== 42)
      $fatal(1, "stops_at_letter was %0d, expected 42", stops_at_letter);
    if (no_digits !== 0)
      $fatal(1, "no_digits was %0d, expected 0", no_digits);
    if (from_blank !== 0)
      $fatal(1, "from_blank was %0d, expected 0", from_blank);

    if (upper_hex !== 255)
      $fatal(1, "upper_hex was %0d, expected 255", upper_hex);
    if (mixed_hex !== 32'hdeadbeef)
      $fatal(1, "mixed_hex was %h, expected deadbeef", mixed_hex);
    if (digits_as_hex !== 291)
      $fatal(1, "digits_as_hex was %0d, expected 291", digits_as_hex);
    if (digits_as_octal !== 83)
      $fatal(1, "digits_as_octal was %0d, expected 83", digits_as_octal);
    if (sevens_as_octal !== 63)
      $fatal(1, "sevens_as_octal was %0d, expected 63", sevens_as_octal);
    if (sevens_as_decimal !== 77)
      $fatal(1, "sevens_as_decimal was %0d, expected 77", sevens_as_decimal);
    if (ones_and_zeros_as_binary !== 10)
      $fatal(1, "ones_and_zeros_as_binary was %0d, expected 10",
             ones_and_zeros_as_binary);
    if (ones_and_zeros_as_decimal !== 1010)
      $fatal(1, "ones_and_zeros_as_decimal was %0d, expected 1010",
             ones_and_zeros_as_decimal);
    $display("All checks passed");
  end
endmodule

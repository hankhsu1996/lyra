// A string variable never holds the character "\0". Converting a string
// literal to one removes every "\0" the literal contains -- the removal is not
// a truncation, so the characters after it stay -- and a literal with nothing
// left over becomes the empty string. An integral value is cast before it
// reaches a string variable: a width that is not a multiple of 8 is first
// zero-filled on the left until it is, each 8 bits of the result is one
// character, and the same removal follows (LRM 6.16, 5.9).
module Top;
  string embedded = "hello\0world";
  string only_nuls = "unset";
  string blank = "unset";
  string leading_zero_byte;
  string two_bytes;

  int embedded_len;
  int only_nuls_len = -1;
  int blank_len = -1;
  int leading_zero_byte_len;

  string odd_width;
  int odd_width_len;
  int odd_width_first;
  int odd_width_second;

  initial begin
    only_nuls = "\0\0";
    blank = "";

    leading_zero_byte = string'(16'h0041);
    two_bytes = string'(16'h4142);

    embedded_len = embedded.len();
    only_nuls_len = only_nuls.len();
    blank_len = blank.len();
    leading_zero_byte_len = leading_zero_byte.len();

    odd_width = string'(12'ha41);
    odd_width_len = odd_width.len();
    odd_width_first = odd_width.getc(0);
    odd_width_second = odd_width.getc(1);
  end

  final begin
    if (embedded != "helloworld")
      $fatal(1, "embedded was \"%s\", expected \"helloworld\"", embedded);
    if (embedded_len !== 10)
      $fatal(1, "embedded_len was %0d, expected 10", embedded_len);

    if (only_nuls != "")
      $fatal(1, "only_nuls was \"%s\", expected \"\"", only_nuls);
    if (only_nuls_len !== 0)
      $fatal(1, "only_nuls_len was %0d, expected 0", only_nuls_len);
    if (blank != "") $fatal(1, "blank was \"%s\", expected \"\"", blank);
    if (blank_len !== 0) $fatal(1, "blank_len was %0d, expected 0", blank_len);

    if (leading_zero_byte != "A")
      $fatal(1, "leading_zero_byte was \"%s\", expected \"A\"",
             leading_zero_byte);
    if (leading_zero_byte_len !== 1)
      $fatal(1, "leading_zero_byte_len was %0d, expected 1",
             leading_zero_byte_len);
    if (two_bytes != "AB")
      $fatal(1, "two_bytes was \"%s\", expected \"AB\"", two_bytes);

    if (odd_width_len !== 2)
      $fatal(1, "odd_width_len was %0d, expected 2", odd_width_len);
    if (odd_width_first !== 8'h0a)
      $fatal(1, "odd_width_first was %h, expected 0a", odd_width_first);
    if (odd_width_second !== 8'h41)
      $fatal(1, "odd_width_second was %h, expected 41", odd_width_second);
    $display("All checks passed");
  end
endmodule

// An argument with no format specification takes the default radix of the task
// it was given to: decimal for $swrite, binary for $swriteb, octal for
// $swriteo and hexadecimal for $swriteh, matching the $write family these are
// based on. An argument of string type with no format specification is
// formatted as a character string even when the task it was given to has a
// numeric default radix (LRM 21.2.1.1, 21.3.3).
module Top;
  logic [7:0] value;
  string word;

  string as_decimal;
  string as_binary;
  string as_hex;
  string as_octal;
  string as_text;

  initial begin
    value = 8'hAB;
    word = "text";

    $swrite(as_decimal, value);
    $swriteb(as_binary, value);
    $swriteh(as_hex, value);
    $swriteo(as_octal, value);
    $swriteh(as_text, word);
  end

  final begin
    if (as_decimal != "171")
      $fatal(1, "$swrite gave '%s', expected the decimal 171", as_decimal);
    if (as_binary != "10101011")
      $fatal(1, "$swriteb gave '%s', expected the binary 10101011", as_binary);
    if (as_hex != "ab")
      $fatal(1, "$swriteh gave '%s', expected the hexadecimal ab", as_hex);
    if (as_octal != "253")
      $fatal(1, "$swriteo gave '%s', expected the octal 253", as_octal);
    if (as_text != "text")
      $fatal(1, "a string argument gave '%s', expected text", as_text);
    $display("All checks passed");
  end
endmodule

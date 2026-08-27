// itoa, hextoa, octtoa and bintoa write an integer into a string as decimal,
// hexadecimal, octal and binary text, replacing whatever the string held. Each
// is the inverse of the method that reads that base back, so the text carries
// the digits of that base and nothing else -- no base prefix and no padding.
// The standard fixes those digits but not whether a hexadecimal digit above 9
// is written upper- or lowercase (LRM 6.16.11, 6.16.12, 6.16.13, 6.16.14).
module Top;
  string decimal;
  string decimal_negative;
  string decimal_zero;
  string hexadecimal;
  string hexadecimal_wide;
  string octal;
  string octal_carry;
  string binary;
  string binary_one;

  integer hexadecimal_read_back = 999;
  integer octal_read_back = 999;
  integer binary_read_back = 999;
  integer decimal_read_back = 999;

  initial begin
    decimal = "unset";
    decimal.itoa(123);
    decimal_negative.itoa(-42);
    decimal_zero.itoa(0);
    hexadecimal.hextoa(255);
    hexadecimal_wide.hextoa(48879);
    octal.octtoa(63);
    octal_carry.octtoa(8);
    binary.bintoa(10);
    binary_one.bintoa(1);

    decimal_read_back = decimal.atoi();
    hexadecimal_read_back = hexadecimal_wide.atohex();
    octal_read_back = octal.atooct();
    binary_read_back = binary.atobin();
  end

  final begin
    if (decimal != "123")
      $fatal(1, "decimal was \"%s\", expected \"123\"", decimal);
    if (decimal_negative != "-42")
      $fatal(1, "decimal_negative was \"%s\", expected \"-42\"",
             decimal_negative);
    if (decimal_zero != "0")
      $fatal(1, "decimal_zero was \"%s\", expected \"0\"", decimal_zero);

    if (hexadecimal.tolower() != "ff")
      $fatal(1, "hexadecimal was \"%s\", expected \"ff\" or \"FF\"",
             hexadecimal);
    if (hexadecimal_wide.tolower() != "beef")
      $fatal(1, "hexadecimal_wide was \"%s\", expected \"beef\" or \"BEEF\"",
             hexadecimal_wide);

    if (octal != "77") $fatal(1, "octal was \"%s\", expected \"77\"", octal);
    if (octal_carry != "10")
      $fatal(1, "octal_carry was \"%s\", expected \"10\"", octal_carry);
    if (binary != "1010")
      $fatal(1, "binary was \"%s\", expected \"1010\"", binary);
    if (binary_one != "1")
      $fatal(1, "binary_one was \"%s\", expected \"1\"", binary_one);

    if (decimal_read_back !== 123)
      $fatal(1, "decimal_read_back was %0d, expected 123", decimal_read_back);
    if (hexadecimal_read_back !== 48879)
      $fatal(1, "hexadecimal_read_back was %0d, expected 48879",
             hexadecimal_read_back);
    if (octal_read_back !== 63)
      $fatal(1, "octal_read_back was %0d, expected 63", octal_read_back);
    if (binary_read_back !== 10)
      $fatal(1, "binary_read_back was %0d, expected 10", binary_read_back);
    $display("All checks passed");
  end
endmodule

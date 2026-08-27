// Each conversion code matches the input field characters Table 21-7 gives it
// and stores the value that field denotes. The numeric codes accept the
// underscore separator and the unknown and high-impedance digits exactly as a
// source description would: %b takes one bit per character, %o three, %h four,
// and %d takes either an optionally signed run of decimal digits or a single
// unknown digit standing for the whole destination. %c matches one character
// and yields its 8-bit ASCII value, and %s matches a run of nonwhitespace
// characters (LRM 21.3.4.3).
module Top;
  bit [3:0] binary_known;
  logic [3:0] binary_unknown;
  bit [8:0] octal_known;
  logic [8:0] octal_unknown;
  int decimal;
  int negative_decimal;
  int separated_decimal;
  logic [7:0] decimal_all_x;
  logic [7:0] decimal_all_z;
  int hexadecimal;
  int separated_hexadecimal;
  logic [7:0] hexadecimal_x_digit;
  logic [7:0] hexadecimal_z_digit;
  byte character;
  string word;

  int matched_total;

  initial begin
    matched_total = 0;
    matched_total += $sscanf("1_0_1_0", "%b", binary_known);
    matched_total += $sscanf("1x0z", "%b", binary_unknown);
    matched_total += $sscanf("377", "%o", octal_known);
    matched_total += $sscanf("3xz", "%o", octal_unknown);
    matched_total += $sscanf("42", "%d", decimal);
    matched_total += $sscanf("-17", "%d", negative_decimal);
    matched_total += $sscanf("1_234_567", "%d", separated_decimal);
    matched_total += $sscanf("x", "%d", decimal_all_x);
    matched_total += $sscanf("z", "%d", decimal_all_z);
    matched_total += $sscanf("dead", "%h", hexadecimal);
    matched_total += $sscanf("d_e_a_d", "%h", separated_hexadecimal);
    matched_total += $sscanf("3x", "%h", hexadecimal_x_digit);
    matched_total += $sscanf("3z", "%h", hexadecimal_z_digit);
    matched_total += $sscanf("A", "%c", character);
    matched_total += $sscanf("hello", "%s", word);
  end

  final begin
    if (matched_total !== 15)
      $fatal(1, "fifteen conversions matched %0d times in all, expected 15",
             matched_total);

    if (binary_known !== 4'b1010)
      $fatal(1, "%%b of 1_0_1_0 was %b, expected 1010", binary_known);
    if (binary_unknown !== 4'b1x0z)
      $fatal(1, "%%b of 1x0z was %b, expected 1x0z", binary_unknown);

    if (octal_known !== 9'b011111111)
      $fatal(1, "%%o of 377 was %b, expected 011111111", octal_known);
    if (octal_unknown !== 9'b011xxxzzz)
      $fatal(1, "%%o of 3xz was %b, expected 011xxxzzz", octal_unknown);

    if (decimal !== 42)
      $fatal(1, "%%d of 42 was %0d, expected 42", decimal);
    if (negative_decimal !== -17)
      $fatal(1, "%%d of -17 was %0d, expected -17", negative_decimal);
    if (separated_decimal !== 1234567)
      $fatal(1, "%%d of 1_234_567 was %0d, expected 1234567",
             separated_decimal);
    if (decimal_all_x !== 8'bxxxxxxxx)
      $fatal(1, "%%d of x was %b, expected every bit unknown", decimal_all_x);
    if (decimal_all_z !== 8'bzzzzzzzz)
      $fatal(1, "%%d of z was %b, expected every bit high impedance",
             decimal_all_z);

    if (hexadecimal !== 32'h0000dead)
      $fatal(1, "%%h of dead was %h, expected 0000dead", hexadecimal);
    if (separated_hexadecimal !== 32'h0000dead)
      $fatal(1, "%%h of d_e_a_d was %h, expected 0000dead",
             separated_hexadecimal);
    if (hexadecimal_x_digit !== 8'b0011xxxx)
      $fatal(1, "%%h of 3x was %b, expected 0011xxxx", hexadecimal_x_digit);
    if (hexadecimal_z_digit !== 8'b0011zzzz)
      $fatal(1, "%%h of 3z was %b, expected 0011zzzz", hexadecimal_z_digit);

    if (character !== 65)
      $fatal(1, "%%c of A was %0d, expected 65", character);
    if (word != "hello")
      $fatal(1, "%%s of hello was '%s', expected 'hello'", word);
    $display("All checks passed");
  end
endmodule

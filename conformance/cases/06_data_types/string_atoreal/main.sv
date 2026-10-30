// atoreal reads a string as a real number. It parses the text the way a real
// constant is written, which admits a decimal point and an exponent, stops at
// the first character that does not fit that syntax or at the end of the
// string, and returns zero when it read no digits at all
// (LRM 6.16.10, 5.7.2).
module Top;
  string decimal_point = "3.14";
  string scientific = "2.5e2";
  string whole = "42";
  string trailing_text = "1.5abc";
  string letters = "abc";
  string blank = "";

  real from_decimal_point = 99.0;
  real from_scientific = 99.0;
  real from_whole = 99.0;
  real from_trailing_text = 99.0;
  real from_letters = 99.0;
  real from_blank = 99.0;

  initial begin
    from_decimal_point = decimal_point.atoreal();
    from_scientific = scientific.atoreal();
    from_whole = whole.atoreal();
    from_trailing_text = trailing_text.atoreal();
    from_letters = letters.atoreal();
    from_blank = blank.atoreal();
  end

  final begin
    if (from_decimal_point != 3.14)
      $fatal(1, "from_decimal_point was %f, expected 3.14",
             from_decimal_point);
    if (from_scientific != 250.0)
      $fatal(1, "from_scientific was %f, expected 250.0", from_scientific);
    if (from_whole != 42.0)
      $fatal(1, "from_whole was %f, expected 42.0", from_whole);
    if (from_trailing_text != 1.5)
      $fatal(1, "from_trailing_text was %f, expected 1.5", from_trailing_text);
    if (from_letters != 0.0)
      $fatal(1, "from_letters was %f, expected 0.0", from_letters);
    if (from_blank != 0.0)
      $fatal(1, "from_blank was %f, expected 0.0", from_blank);
    $display("All checks passed");
  end
endmodule

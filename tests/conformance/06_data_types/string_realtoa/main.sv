// realtoa writes a real number into a string and is the inverse of atoreal, so
// the text it writes reads back as the number it was given. The standard fixes
// that relation but not how the text is spelled, and the relation holds only
// if something is written and two different numbers are written differently
// (LRM 6.16.15, 6.16.10).
module Top;
  string fraction_text;
  string whole_text;

  real fraction_read_back = 99.0;
  real whole_read_back = 99.0;
  int fraction_len;
  int whole_len;
  bit spellings_differ;

  initial begin
    fraction_text.realtoa(3.14);
    whole_text.realtoa(250.0);

    fraction_read_back = fraction_text.atoreal();
    whole_read_back = whole_text.atoreal();
    fraction_len = fraction_text.len();
    whole_len = whole_text.len();
    spellings_differ = (fraction_text != whole_text);
  end

  final begin
    if (fraction_read_back != 3.14)
      $fatal(1, "3.14 read back as %f", fraction_read_back);
    if (whole_read_back != 250.0)
      $fatal(1, "250.0 read back as %f", whole_read_back);
    if (fraction_len === 0) $fatal(1, "realtoa wrote nothing for 3.14");
    if (whole_len === 0) $fatal(1, "realtoa wrote nothing for 250.0");
    if (spellings_differ !== 1'b1)
      $fatal(1, "3.14 and 250.0 were both written as \"%s\"", fraction_text);
    $display("All checks passed");
  end
endmodule

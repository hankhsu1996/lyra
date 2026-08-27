// Writing a character replaces the one already at that position and never
// changes the string's length: str.putc(i, c) and str[i] = c are the same
// write. The value written is the low 8 bits of the expression, so a longer
// string literal on the right contributes only its last character. The write
// is ignored when i is below 0 or at or past the length, and ignored when the
// character is 0, because a string variable never holds "\0"
// (LRM 6.16, 6.16.2, Table 6-9).
module Top;
  string s = "Hello";

  int len_after_writes;
  int untouched_by_zero;
  int untouched_by_nul_literal;

  initial begin
    s.putc(1, "x");
    s[4] = "O";
    s[0] = "cough";

    s.putc(2, 8'h00);
    s[3] = "\0";
    s.putc(5, "Z");
    s.putc(-1, "Z");
    s[99] = "Z";

    len_after_writes = s.len();
    untouched_by_zero = s.getc(2);
    untouched_by_nul_literal = s.getc(3);
  end

  final begin
    if (s != "hxllO") $fatal(1, "s was \"%s\", expected \"hxllO\"", s);
    if (len_after_writes !== 5)
      $fatal(1, "len_after_writes was %0d, expected 5", len_after_writes);
    if (untouched_by_zero !== 108)
      $fatal(1, "untouched_by_zero was %0d, expected 108", untouched_by_zero);
    if (untouched_by_nul_literal !== 108)
      $fatal(1, "untouched_by_nul_literal was %0d, expected 108",
             untouched_by_nul_literal);
    $display("All checks passed");
  end
endmodule

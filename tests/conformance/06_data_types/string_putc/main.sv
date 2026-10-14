// Writing a character replaces the one already at that position and never
// changes the string's length: str.putc(i, c) and str[i] = c are the same
// write. The value written is the low 8 bits of the expression, so a longer
// string literal on the right contributes only its last character. The write
// is ignored when i is below 0 or at or past the length, and ignored when the
// character is 0, because a string variable never holds "\0"
// (LRM 6.16, 6.16.2, Table 6-9). The index is the `int` argument of putc, so
// an x or z bit of it reads as 0 (LRM 6.11.2). A character is a `byte`, so a
// write may name some of its bits and leaves the rest of the character as it
// was (LRM 11.5.1). A character is written the same way from every position a
// write takes -- an operator assignment and an increment, which read it first
// (LRM 11.4.1, 11.4.2), a nonblocking assignment, an output argument, a member
// of a concatenation -- and in a string an automatic body declares as in a
// module's.
module Top;
  string s = "Hello";
  string t = "abc";
  string u = "abcd";
  string late = "xyz";
  logic [3:0] all_unknown = 4'bxxxx;
  int before_first = -7;

  int len_after_writes;
  int untouched_by_zero;
  int untouched_by_nul_literal;
  string body_text;

  task automatic give(output byte c);
    c = "Q";
  endtask

  function automatic string in_a_body(int at, int nowhere);
    string local_text = "mnop";
    local_text[at] = "M";
    local_text[1][5] = 1'b0;
    local_text[2] += 2;
    local_text[3]++;
    local_text[nowhere] = "W";
    return local_text;
  endfunction

  initial begin
    s.putc(1, "x");
    s[4] = "O";
    s[0] = "cough";

    s.putc(2, 8'h00);
    s[3] = "\0";
    s.putc(5, "Z");
    s.putc(-1, "Z");
    s[99] = "Z";
    t[all_unknown] = "Z";

    len_after_writes = s.len();
    untouched_by_zero = s.getc(2);
    untouched_by_nul_literal = s.getc(3);

    u[0][5] = 1'b0;
    u[1][3:0] = 4'h4;
    u[2] += 1;
    u[3]++;
    give(u[1]);
    {u[2], u[3]} = "ST";
    late[0] <= "L";
    body_text = in_a_body(0, before_first);
  end

  final begin
    if (s != "hxllO") $fatal(1, "s was \"%s\", expected \"hxllO\"", s);
    if (t != "Zbc") $fatal(1, "t was \"%s\", expected \"Zbc\"", t);
    if (len_after_writes !== 5)
      $fatal(1, "len_after_writes was %0d, expected 5", len_after_writes);
    if (untouched_by_zero !== 108)
      $fatal(1, "untouched_by_zero was %0d, expected 108", untouched_by_zero);
    if (untouched_by_nul_literal !== 108)
      $fatal(1, "untouched_by_nul_literal was %0d, expected 108",
             untouched_by_nul_literal);
    if (u != "AQST") $fatal(1, "u was \"%s\", expected \"AQST\"", u);
    if (late != "Lyz") $fatal(1, "late was \"%s\", expected \"Lyz\"", late);
    if (body_text != "MNqq")
      $fatal(1, "body_text was \"%s\", expected \"MNqq\"", body_text);
    $display("All checks passed");
  end
endmodule

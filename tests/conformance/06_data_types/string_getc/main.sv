// A string's characters are numbered from 0 for the leftmost to N-1 for the
// rightmost. Reading one yields its ASCII code, and str[i] and str.getc(i) are
// the same read. An index below 0, or at or past the length, is out of range
// and reads 0; every index of an empty string is out of range on those terms
// (LRM 6.16, 6.16.3, Table 6-9).
module Top;
  string s = "Hello";
  string empty = "";

  int first = 99;
  int last = 99;
  int by_index = 99;
  int by_method = 99;
  int negative_index = 99;
  int at_length = 99;
  int past_end = 99;
  int in_empty = 99;

  initial begin
    first = s.getc(0);
    last = s.getc(4);
    by_index = s[1];
    by_method = s.getc(1);
    negative_index = s.getc(-1);
    at_length = s.getc(5);
    past_end = s[99];
    in_empty = empty[0];
  end

  final begin
    if (first !== 72) $fatal(1, "first was %0d, expected 72", first);
    if (last !== 111) $fatal(1, "last was %0d, expected 111", last);
    if (by_index !== 101)
      $fatal(1, "by_index was %0d, expected 101", by_index);
    if (by_method !== 101)
      $fatal(1, "by_method was %0d, expected 101", by_method);
    if (negative_index !== 0)
      $fatal(1, "negative_index was %0d, expected 0", negative_index);
    if (at_length !== 0)
      $fatal(1, "at_length was %0d, expected 0", at_length);
    if (past_end !== 0) $fatal(1, "past_end was %0d, expected 0", past_end);
    if (in_empty !== 0) $fatal(1, "in_empty was %0d, expected 0", in_empty);
    $display("All checks passed");
  end
endmodule

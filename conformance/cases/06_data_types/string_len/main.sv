// The len method returns the number of characters a string holds, and zero for
// the empty string. Every character counts, including a space and one written
// as an escape sequence, which is a single 8-bit ASCII value however many
// source characters spell it. The count follows the value the variable holds,
// so it grows with a concatenation and is unchanged by a write that replaces
// one character (LRM 6.16, 6.16.1, 5.9).
module Top;
  string word = "hello";
  string with_spaces = "a b c";
  string escaped = "a\tb";
  string empty = "";
  string joined;

  int word_len;
  int with_spaces_len;
  int escaped_len;
  int empty_len = -1;
  int joined_len;
  int len_after_write;

  initial begin
    word_len = word.len();
    with_spaces_len = with_spaces.len();
    escaped_len = escaped.len();
    empty_len = empty.len();

    joined = {word, " ", "world"};
    joined_len = joined.len();

    word.putc(0, "H");
    len_after_write = word.len();
  end

  final begin
    if (word_len !== 5) $fatal(1, "word_len was %0d, expected 5", word_len);
    if (with_spaces_len !== 5)
      $fatal(1, "with_spaces_len was %0d, expected 5", with_spaces_len);
    if (escaped_len !== 3)
      $fatal(1, "escaped_len was %0d, expected 3", escaped_len);
    if (empty_len !== 0) $fatal(1, "empty_len was %0d, expected 0", empty_len);
    if (joined_len !== 11)
      $fatal(1, "joined_len was %0d, expected 11", joined_len);
    if (len_after_write !== 5)
      $fatal(1, "len_after_write was %0d, expected 5", len_after_write);
    $display("All checks passed");
  end
endmodule

// A conversion specification is a %, an optional assignment suppression
// character *, an optional maximum field width, and a conversion code. An
// input field extends to the next character the code cannot take or until the
// maximum field width is exhausted, whichever comes first. A suppressed
// conversion consumes its field and assigns nothing, so it does not count
// toward the number returned (LRM 21.3.4.3).
module Top;
  int decimal;
  int hexadecimal;
  int binary;
  string word;
  int width_count;

  int truncated;
  int truncated_count;

  int before_suppressed;
  int after_suppressed;
  int suppressed_count;

  int wide_before_suppressed;
  int wide_after_suppressed;
  int mixed_count;

  initial begin
    width_count = $sscanf("123 abc 1111 hello world", "%3d %3h %4b %5s",
                          decimal, hexadecimal, binary, word);
    truncated_count = $sscanf("12345", "%3d", truncated);
    suppressed_count = $sscanf("10 20 30", "%d %*d %d", before_suppressed,
                               after_suppressed);
    mixed_count = $sscanf("999 88 7", "%3d %*d %d", wide_before_suppressed,
                          wide_after_suppressed);
  end

  final begin
    if (width_count !== 4)
      $fatal(1, "four width-capped conversions returned %0d, expected 4",
             width_count);
    if (decimal !== 123)
      $fatal(1, "%%3d was %0d, expected 123", decimal);
    if (hexadecimal !== 32'h0abc)
      $fatal(1, "%%3h was %h, expected abc", hexadecimal);
    if (binary !== 15)
      $fatal(1, "%%4b of 1111 was %0d, expected 15", binary);
    if (word != "hello")
      $fatal(1, "%%5s was '%s', expected 'hello'", word);

    if (truncated_count !== 1)
      $fatal(1, "a width-capped conversion returned %0d, expected 1",
             truncated_count);
    if (truncated !== 123)
      $fatal(1, "%%3d of 12345 was %0d, expected 123", truncated);

    if (suppressed_count !== 2)
      $fatal(1, "two assigned and one suppressed returned %0d, expected 2",
             suppressed_count);
    if (before_suppressed !== 10)
      $fatal(1, "the field before the suppressed one was %0d, expected 10",
             before_suppressed);
    if (after_suppressed !== 30)
      $fatal(1, "the field after the suppressed one was %0d, expected 30",
             after_suppressed);

    if (mixed_count !== 2)
      $fatal(1, "a width and a suppression together returned %0d, expected 2",
             mixed_count);
    if (wide_before_suppressed !== 999)
      $fatal(1, "the width-capped field was %0d, expected 999",
             wide_before_suppressed);
    if (wide_after_suppressed !== 7)
      $fatal(1, "the field after the suppressed one was %0d, expected 7",
             wide_after_suppressed);
    $display("All checks passed");
  end
endmodule

// $fscanf reads from the file a descriptor names, interpreting what it reads
// by the same conversion specifications $sscanf uses, and returns the number of
// items it matched and assigned -- EOF when the input ends before any of them.
// When a conversion stops on a conflicting input character, that character is
// left unread in the stream, so the next read entry sees it (LRM 21.3.4.3).
module Top;
  int fd;

  int across_codes;
  int decimal;
  logic [15:0] hexadecimal;
  string word;

  int at_end;
  int untouched_by_end;

  int partial;
  int matched_value;
  int unreached_value;

  int up_to_literal;
  int before_literal;
  int unreached_by_literal;
  int leftover;

  initial begin
    fd = $fopen("scan.txt", "w");
    $fwrite(fd, "42 dead hello");
    $fclose(fd);
    fd = $fopen("scan.txt", "r");
    across_codes = $fscanf(fd, "%d %h %s", decimal, hexadecimal, word);
    $fclose(fd);

    fd = $fopen("empty.txt", "w");
    $fclose(fd);
    untouched_by_end = 99;
    fd = $fopen("empty.txt", "r");
    at_end = $fscanf(fd, "%d", untouched_by_end);
    $fclose(fd);

    fd = $fopen("partial.txt", "w");
    $fwrite(fd, "12 abc");
    $fclose(fd);
    unreached_value = 99;
    fd = $fopen("partial.txt", "r");
    partial = $fscanf(fd, "%d %d", matched_value, unreached_value);
    $fclose(fd);

    fd = $fopen("literal.txt", "w");
    $fwrite(fd, "12x34");
    $fclose(fd);
    fd = $fopen("literal.txt", "r");
    up_to_literal = $fscanf(fd, "%d:%d", before_literal,
                            unreached_by_literal);
    leftover = $fgetc(fd);
    $fclose(fd);
  end

  final begin
    if (across_codes !== 3)
      $fatal(1, "three conversions from a file returned %0d, expected 3",
             across_codes);
    if (decimal !== 42)
      $fatal(1, "the decimal value was %0d, expected 42", decimal);
    if (hexadecimal !== 16'hdead)
      $fatal(1, "the hexadecimal value was %h, expected dead", hexadecimal);
    if (word != "hello")
      $fatal(1, "the word was '%s', expected 'hello'", word);

    if (at_end !== -1)
      $fatal(1, "an empty file returned %0d, expected -1", at_end);
    if (untouched_by_end !== 99)
      $fatal(1, "the output after an empty file was %0d, expected 99",
             untouched_by_end);

    if (partial !== 1)
      $fatal(1, "one of two conversions matching returned %0d, expected 1",
             partial);
    if (matched_value !== 12)
      $fatal(1, "the matched value was %0d, expected 12", matched_value);

    if (up_to_literal !== 1)
      $fatal(1, "an unmatched literal returned %0d, expected 1",
             up_to_literal);
    if (before_literal !== 12)
      $fatal(1, "the value before the literal was %0d, expected 12",
             before_literal);
    if (leftover !== 120)
      $fatal(1, "the byte left unread was %0d, expected 120", leftover);
    $display("All checks passed");
  end
endmodule

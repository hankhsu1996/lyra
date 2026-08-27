// $fgets reads characters into a string until a newline is read and
// transferred into it, or until end of file, and returns the number of
// characters read. The newline that ends a line is part of what it delivers; a
// final line without one is delivered without it (LRM 21.3.4.2).
module Top;
  int fd;
  string first_line;
  string second_line;
  int first_count;
  int second_count;

  initial begin
    fd = $fopen("lines.txt", "w");
    $fwrite(fd, "hello\ntail");
    $fclose(fd);

    fd = $fopen("lines.txt", "r");
    first_count = $fgets(first_line, fd);
    second_count = $fgets(second_line, fd);
    $fclose(fd);
  end

  final begin
    if (first_count !== 6)
      $fatal(1, "the first line's count was %0d, expected 6", first_count);
    if (first_line != "hello\n")
      $fatal(1, "the first line was '%s', expected it to keep its newline",
             first_line);
    if (second_count !== 4)
      $fatal(1, "the second line's count was %0d, expected 4", second_count);
    if (second_line != "tail")
      $fatal(1, "the second line was '%s', expected 'tail'", second_line);
    $display("All checks passed");
  end
endmodule

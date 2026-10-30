// $ftell returns the offset from the start of the file of the byte a
// subsequent operation reads or writes. $fseek moves that position to an
// offset counted from the start of the file, from the current position, or
// from its end, according to an operation of 0, 1 or 2, and returns 0 when it
// succeeds. $rewind is $fseek with an offset of 0 counted from the start
// (LRM 21.3.5).
module Top;
  int fd;
  int at_open;
  int after_three_reads;

  int absolute_result;
  int after_absolute;
  int byte_at_seven;

  int end_result;
  int at_end;

  int rewind_result;
  int after_rewind;
  int first_byte_again;

  int relative_result;
  int after_relative;
  int byte_at_four;

  initial begin
    fd = $fopen("positions.txt", "w");
    $fwrite(fd, "0123456789");
    $fclose(fd);

    fd = $fopen("positions.txt", "r");
    at_open = $ftell(fd);

    void'($fgetc(fd));
    void'($fgetc(fd));
    void'($fgetc(fd));
    after_three_reads = $ftell(fd);

    absolute_result = $fseek(fd, 7, 0);
    after_absolute = $ftell(fd);
    byte_at_seven = $fgetc(fd);

    relative_result = $fseek(fd, -4, 1);
    after_relative = $ftell(fd);
    byte_at_four = $fgetc(fd);

    end_result = $fseek(fd, 0, 2);
    at_end = $ftell(fd);

    rewind_result = $rewind(fd);
    after_rewind = $ftell(fd);
    first_byte_again = $fgetc(fd);
    $fclose(fd);
  end

  final begin
    if (at_open !== 0)
      $fatal(1, "$ftell on a freshly opened file was %0d, expected 0",
             at_open);
    if (after_three_reads !== 3)
      $fatal(1, "$ftell after three reads was %0d, expected 3",
             after_three_reads);

    if (absolute_result !== 0)
      $fatal(1, "seeking from the start returned %0d, expected 0",
             absolute_result);
    if (after_absolute !== 7)
      $fatal(1, "$ftell after seeking to 7 was %0d, expected 7",
             after_absolute);
    if (byte_at_seven !== 55)
      $fatal(1, "the byte at offset 7 was %0d, expected 55", byte_at_seven);

    if (end_result !== 0)
      $fatal(1, "seeking from the end returned %0d, expected 0", end_result);
    if (at_end !== 10)
      $fatal(1, "$ftell at the end of a ten-byte file was %0d, expected 10",
             at_end);

    if (rewind_result !== 0)
      $fatal(1, "$rewind returned %0d, expected 0", rewind_result);
    if (after_rewind !== 0)
      $fatal(1, "$ftell after $rewind was %0d, expected 0", after_rewind);
    if (first_byte_again !== 48)
      $fatal(1, "the byte after $rewind was %0d, expected 48",
             first_byte_again);

    if (relative_result !== 0)
      $fatal(1, "seeking from the current position returned %0d, expected 0",
             relative_result);
    if (after_relative !== 4)
      $fatal(1, "$ftell after seeking back four was %0d, expected 4",
             after_relative);
    if (byte_at_four !== 52)
      $fatal(1, "the byte at offset 4 was %0d, expected 52", byte_at_four);
    $display("All checks passed");
  end
endmodule

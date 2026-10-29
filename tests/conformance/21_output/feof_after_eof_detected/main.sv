// $feof returns a nonzero value once end of file has been detected reading a
// descriptor, and zero otherwise. Reading the last byte a file holds does not
// detect it; the read that finds nothing left does (LRM 21.3.8, 21.3.4.1).
//
// A seek to a position before the file's start cannot reposition it and
// answers -1; one that does reposition it sets the position of the next read,
// so nothing has been detected there yet (LRM 21.3.5).
module Top;
  int fd;
  int at_open;
  int only_byte;
  int after_last_byte;
  int past_the_end;
  int after_eof;
  int failed_seek;
  int after_seek;

  initial begin
    fd = $fopen("eof.txt", "w");
    $fwrite(fd, "x");
    $fclose(fd);

    fd = $fopen("eof.txt", "r");
    at_open = $feof(fd);
    only_byte = $fgetc(fd);
    after_last_byte = $feof(fd);
    past_the_end = $fgetc(fd);
    after_eof = $feof(fd);
    failed_seek = $fseek(fd, -1, 0);
    void'($fseek(fd, 0, 0));
    after_seek = $feof(fd);
    $fclose(fd);
  end

  final begin
    if (at_open !== 0)
      $fatal(1, "$feof on a freshly opened file was %0d, expected 0", at_open);
    if (only_byte !== 120)
      $fatal(1, "the only byte was %0d, expected 120", only_byte);
    if (after_last_byte !== 0)
      $fatal(1, "$feof after reading the last byte was %0d, expected 0",
             after_last_byte);
    if (past_the_end !== -1)
      $fatal(1, "$fgetc past the end returned %0d, expected -1", past_the_end);
    if (after_eof === 0)
      $fatal(1, "$feof after end of file was 0, expected a nonzero value");
    if (failed_seek !== -1)
      $fatal(1, "$fseek before the file start returned %0d, expected -1",
             failed_seek);
    if (after_seek !== 0)
      $fatal(1, "$feof after a seek to the start was %0d, expected 0",
             after_seek);
    $display("All checks passed");
  end
endmodule

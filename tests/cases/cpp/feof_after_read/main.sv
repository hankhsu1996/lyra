module Top;
  int fd;
  int eof_before_read;
  int byte_one;
  int eof_after_one;
  int byte_two;
  int eof_after_eof_get;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "x");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    // LRM 21.3.8: $feof returns nonzero only after an EOF has been previously
    // detected. With the file just opened, no read has happened yet.
    eof_before_read = $feof(fd);

    byte_one = $fgetc(fd);             // 'x'
    eof_after_one = $feof(fd);         // still zero -- the one byte was read,
                                       // EOF not yet detected.

    byte_two = $fgetc(fd);             // -1 (EOF)
    eof_after_eof_get = $feof(fd);     // now nonzero.

    $fclose(fd);
  end
endmodule

module Top;
  int fd;
  string first_line;
  string second_line;
  int first_count;
  int second_count;
  int first_len;
  int second_len;
  int first_last_byte;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fdisplay(fd, "hello");        // writes "hello\n"
    $fwrite(fd, "tail");           // writes "tail" (no newline)
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    // LRM 21.3.4.2: $fgets returns the byte count and the trailing newline
    // is kept in the result (when present). The probe protocol can't carry
    // embedded newlines, so we cross-check via len() and the last byte.
    first_count = $fgets(first_line, fd);
    second_count = $fgets(second_line, fd);
    $fclose(fd);

    first_len = first_line.len();
    second_len = second_line.len();
    first_last_byte = first_line.getc(first_len - 1);
  end
endmodule

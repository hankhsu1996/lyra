module Top;
  // LRM 21.3.4.1 happy path: the byte $ungetc deposits is the next byte
  // any read entry sees.
  int basic_first;
  int basic_unget_ret;
  int basic_second;
  bit [15:0] read_after_unget;
  string gets_after_unget;
  // LRM 21.3.4.1 "if an error occurs pushing a character ... code is set
  // to EOF" -- the buffer is single-char, a second $ungetc before any
  // read returns -1.
  int unget_first_ret;
  int unget_second_ret;
  // LRM 21.3.5: "Repositioning the current file position with $fseek or
  // $rewind shall cancel any $ungetc operations."
  int after_seek;
  int after_rewind;
  initial begin
    int fd;
    int unused;

    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "AB");
    $fclose(fd);

    // $fgetc consumes the pushback before the file.
    fd = $fopen("scratch.txt", "r");
    basic_first = $fgetc(fd);             // 'A' = 65
    basic_unget_ret = $ungetc(90, fd);    // push 'Z' = 90
    basic_second = $fgetc(fd);            // 90 (pushback), not 'B'
    $fclose(fd);

    // Channel-wide putback: $fread sees the pushback as its first byte.
    // Pushback 'X' = 0x58 then 'A' = 0x41 -> 16-bit MSB-first = 0x5841.
    fd = $fopen("scratch.txt", "r");
    unused = $ungetc(8'h58, fd);
    unused = $fread(read_after_unget, fd);
    $fclose(fd);

    // Channel-wide putback: $fgets prepends the pushback to its line.
    fd = $fopen("scratch.txt", "r");
    unused = $ungetc(8'h58, fd);
    unused = $fgets(gets_after_unget, fd);  // "XAB"
    $fclose(fd);

    // Single-char buffer: second $ungetc without intervening read fails.
    fd = $fopen("scratch.txt", "r");
    unget_first_ret = $ungetc(8'h41, fd);   // 0 (success)
    unget_second_ret = $ungetc(8'h42, fd);  // -1 (buffer full)
    $fclose(fd);

    // $fseek after $ungetc cancels the pushback (LRM 21.3.5).
    fd = $fopen("scratch.txt", "r");
    unused = $fgetc(fd);
    unused = $ungetc(8'h58, fd);
    unused = $fseek(fd, 0, 0);
    after_seek = $fgetc(fd);                // 'A' = 65, not 'X'
    $fclose(fd);

    // $rewind same cancel guarantee.
    fd = $fopen("scratch.txt", "r");
    unused = $fgetc(fd);
    unused = $ungetc(8'h58, fd);
    unused = $rewind(fd);
    after_rewind = $fgetc(fd);              // 65
    $fclose(fd);
  end
endmodule

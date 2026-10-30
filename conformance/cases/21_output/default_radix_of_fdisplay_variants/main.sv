// $fdisplay and its radix-suffixed forms give an argument with no format
// specification the same default radix their $display counterparts do, and an
// explicit conversion written in a format string is honoured as written rather
// than being reinterpreted in the suffix's radix (LRM 21.2.1.1, 21.3.2).
module Top;
  logic [7:0] value;
  logic [3:0] nibble;
  int write_fd;
  int read_fd;

  string as_decimal;
  string as_binary;
  string as_hex;
  string as_octal;
  string explicit_decimal;
  string explicit_binary;

  initial begin
    value = 8'hAB;
    nibble = 4'b1010;

    write_fd = $fopen("fdisplay_radix.txt", "w");
    $fdisplay(write_fd, value);
    $fdisplayb(write_fd, value);
    $fdisplayh(write_fd, value);
    $fdisplayo(write_fd, value);
    $fdisplayb(write_fd, "decimal %0d", value);
    $fdisplayh(write_fd, "binary %b", nibble);
    $fclose(write_fd);

    read_fd = $fopen("fdisplay_radix.txt", "r");
    void'($fgets(as_decimal, read_fd));
    void'($fgets(as_binary, read_fd));
    void'($fgets(as_hex, read_fd));
    void'($fgets(as_octal, read_fd));
    void'($fgets(explicit_decimal, read_fd));
    void'($fgets(explicit_binary, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (as_decimal != "171\n")
      $fatal(1, "$fdisplay wrote '%s', expected the decimal 171", as_decimal);
    if (as_binary != "10101011\n")
      $fatal(1, "$fdisplayb wrote '%s', expected the binary 10101011",
             as_binary);
    if (as_hex != "ab\n")
      $fatal(1, "$fdisplayh wrote '%s', expected the hexadecimal ab", as_hex);
    if (as_octal != "253\n")
      $fatal(1, "$fdisplayo wrote '%s', expected the octal 253", as_octal);
    if (explicit_decimal != "decimal 171\n")
      $fatal(1, "an explicit decimal wrote '%s', expected decimal 171",
             explicit_decimal);
    if (explicit_binary != "binary 1010\n")
      $fatal(1, "an explicit binary wrote '%s', expected binary 1010",
             explicit_binary);
    $display("All checks passed");
  end
endmodule

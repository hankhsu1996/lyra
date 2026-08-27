// $fstrobe and its radix-suffixed forms take their arguments in the same
// manner as $display, so an argument with no format specification takes the
// default radix of the suffix and is reported at the end of the time step in
// which the task ran (LRM 21.2.2, 21.2.1.1, 21.3.2). One call per time step
// fixes the order the four reports reach the file.
module Top;
  logic [7:0] value;
  int write_fd;
  int read_fd;

  string as_decimal;
  string as_binary;
  string as_hex;
  string as_octal;

  initial begin
    value = 8'hAB;
    write_fd = $fopen("fstrobe_radix.txt", "w");
    $fstrobe(write_fd, value);
    #1 $fstrobeb(write_fd, value);
    #1 $fstrobeh(write_fd, value);
    #1 $fstrobeo(write_fd, value);
    #1 $fclose(write_fd);

    read_fd = $fopen("fstrobe_radix.txt", "r");
    void'($fgets(as_decimal, read_fd));
    void'($fgets(as_binary, read_fd));
    void'($fgets(as_hex, read_fd));
    void'($fgets(as_octal, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (as_decimal != "171\n")
      $fatal(1, "$fstrobe wrote '%s', expected the decimal 171", as_decimal);
    if (as_binary != "10101011\n")
      $fatal(1, "$fstrobeb wrote '%s', expected the binary 10101011",
             as_binary);
    if (as_hex != "ab\n")
      $fatal(1, "$fstrobeh wrote '%s', expected the hexadecimal ab", as_hex);
    if (as_octal != "253\n")
      $fatal(1, "$fstrobeo wrote '%s', expected the octal 253", as_octal);
    $display("All checks passed");
  end
endmodule

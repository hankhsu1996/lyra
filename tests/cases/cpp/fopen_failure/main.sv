module Top;
  int fd;
  int got_zero;
  initial begin
    // LRM 21.3.1: $fopen returns 0 when the file cannot be opened.
    fd = $fopen("/nonexistent/path/to/file.txt", "r");
    got_zero = (fd == 0) ? 1 : 0;
  end
endmodule

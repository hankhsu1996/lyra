module Top;
  int fd_write, fd_read;
  int read_fd_valid;
  initial begin
    // First create the file so it can be opened in read mode.
    fd_write = $fopen("input.txt", "w");
    $fdisplay(fd_write, "data");
    $fclose(fd_write);
    // LRM 21.3.1: $fopen with "r" returns a real FD (bit 31 set) when the
    // file exists. The read-side tasks themselves are out of scope for this
    // PR; this test only proves the FD shape and successful open.
    fd_read = $fopen("input.txt", "r");
    read_fd_valid = (fd_read < 0) ? 1 : 0;
    $fclose(fd_read);
  end
endmodule

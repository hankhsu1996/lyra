module Top;
  int fd;
  int bit31_set;
  initial begin
    fd = $fopen("fd_check.txt", "w");
    // LRM 21.3.1: FD's MSB (bit 31) is always set. Interpreted as a signed
    // int32, the value is negative.
    bit31_set = (fd < 0) ? 1 : 0;
    $fclose(fd);
  end
endmodule

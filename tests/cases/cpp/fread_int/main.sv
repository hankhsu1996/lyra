module Top;
  int fd;
  int read_count;
  int value;
  initial begin
    // Write four bytes 0xDE 0xAD 0xBE 0xEF.
    fd = $fopen("scratch.bin", "wb");
    $fwrite(fd, "%c%c%c%c", 8'hDE, 8'hAD, 8'hBE, 8'hEF);
    $fclose(fd);

    // LRM 21.3.4.4: $fread is big-endian; the first byte fills the MSBs,
    // so the 32-bit int reads as 0xDEADBEEF.
    fd = $fopen("scratch.bin", "rb");
    read_count = $fread(value, fd);
    $fclose(fd);
  end
endmodule

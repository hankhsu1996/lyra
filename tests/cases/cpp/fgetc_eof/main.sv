module Top;
  int fd;
  int after_read;
  int after_eof;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "x");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    after_read = $fgetc(fd);     // 'x' = 120
    after_eof = $fgetc(fd);      // LRM: EOF returns -1
    $fclose(fd);
  end
endmodule

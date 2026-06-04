module Top;
  int fd;
  initial begin
    fd = $fopen("out.txt", "w");
    $fstrobe(fd, "should not appear");
    $fclose(fd);
  end
endmodule

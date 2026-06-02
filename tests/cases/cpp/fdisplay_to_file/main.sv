module Top;
  int fd;
  initial begin
    fd = $fopen("out.txt", "w");
    $fdisplay(fd, "hello");
    $fdisplay(fd, "world");
    $fclose(fd);
  end
endmodule

module Top;
  int fd;
  int code;
  int x;
  initial begin
    fd = $fopen("input.txt", "w");
    $fwrite(fd, "12345");
    $fclose(fd);
    fd = $fopen("input.txt", "r");
    code = $fscanf(fd, "%5d", x);
    $fclose(fd);
  end
endmodule

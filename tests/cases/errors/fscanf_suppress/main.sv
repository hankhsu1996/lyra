module Top;
  int fd;
  int code;
  int a, b;
  initial begin
    fd = $fopen("input.txt", "w");
    $fwrite(fd, "1 2 3");
    $fclose(fd);
    fd = $fopen("input.txt", "r");
    code = $fscanf(fd, "%d %*d %d", a, b);
    $fclose(fd);
  end
endmodule

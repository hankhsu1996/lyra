module Top;
  int fd;
  int first;
  int second;
  int third;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "abc");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    first = $fgetc(fd);
    second = $fgetc(fd);
    third = $fgetc(fd);
    $fclose(fd);
  end
endmodule

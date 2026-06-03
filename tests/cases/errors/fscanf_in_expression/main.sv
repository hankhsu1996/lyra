module Top;
  int fd;
  int a;
  initial begin
    fd = $fopen("input.txt", "r");
    if ($fscanf(fd, "%d", a)) begin
      a = a + 1;
    end
    $fclose(fd);
  end
endmodule

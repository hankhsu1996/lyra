module Top;
  int fd;
  initial begin
    fd = $fopen("out.txt", "w");
    $fstrobe (fd, 8'hAB);
    $fstrobeb(fd, 8'hAB);
    $fstrobeh(fd, 8'hAB);
    $fstrobeo(fd, 8'hAB);
    #1 $fclose(fd);
  end
endmodule

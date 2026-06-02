module Top;
  int fd;
  initial begin
    fd = $fopen("out.txt", "w");
    $fwrite(fd, "line1");
    $fdisplay(fd);
    $fwrite(fd, "line2");
    $fdisplay(fd);
    $fclose(fd);
  end
endmodule

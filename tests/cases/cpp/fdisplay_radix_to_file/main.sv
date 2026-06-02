module Top;
  int fd;
  initial begin
    fd = $fopen("out.txt", "w");
    // Default radix from the variant flows through to the file sink the same
    // way it does to stdout: bare-arg formatting honors LRM 21.2.1.1.
    $fdisplayh(fd, 8'hAB);
    $fdisplayb(fd, 4'h5);
    $fdisplayo(fd, 8'h3F);
    $fclose(fd);
  end
endmodule

module Top;
  logic clk;
  int   cnt;
  int   fd;
  initial begin
    fd = $fopen("out.txt", "w");
    clk = 0;
    cnt = 0;
    #1 clk = 1;
    #1 $fclose(fd);
  end
  always @(posedge clk) begin
    cnt <= cnt + 1;
    $fdisplay(fd, "fdisplay cnt=%0d", cnt);
    $fstrobe (fd, "fstrobe  cnt=%0d", cnt);
  end
endmodule

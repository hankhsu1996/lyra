module Top;
  int fd1, fd2;
  initial begin
    fd1 = $fopen("a.txt", "w");
    fd2 = $fopen("b.txt", "w");
    $fstrobe(fd1, "from fd1");
    $fstrobe(fd2, "from fd2");
    $fclose(fd1);
    #1 $fclose(fd2);
  end
endmodule

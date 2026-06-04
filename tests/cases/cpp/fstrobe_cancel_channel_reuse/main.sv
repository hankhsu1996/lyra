module Top;
  int fd1, fd2;
  initial begin
    fd1 = $fopen("a.txt", "w");
    $fstrobe(fd1, "from first open");
    $fclose(fd1);
    // LRM 21.3.1: $fopen reuses closed channels; fd2 will equal fd1 numerically.
    fd2 = $fopen("b.txt", "w");
    $fstrobe(fd2, "from second open");
    #1 $fclose(fd2);
  end
endmodule

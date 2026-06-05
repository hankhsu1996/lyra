module Top;
  // Same expression-position coverage as the $sscanf case, exercising the
  // file-source path: $fscanf in if-condition, arithmetic, and case selector.
  int fd;
  int if_a;
  int if_taken;
  int arith_a, arith_b;
  int arith_sum;
  int case_a;
  string case_branch;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "42 11 22 7");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    if ($fscanf(fd, "%d", if_a)) begin
      if_taken = if_a + 1;
    end
    arith_sum = $fscanf(fd, "%d %d", arith_a, arith_b) * 100 + arith_a + arith_b;
    case_branch = "none";
    case ($fscanf(fd, "%d", case_a))
      1: case_branch = "one";
      2: case_branch = "two";
      default: case_branch = "other";
    endcase
    $fclose(fd);
  end
endmodule

// $fopen given no type opens the file for writing and returns a multichannel
// descriptor, one per file opened. Bitwise OR-ing two of them makes a
// descriptor that directs one output task's text to both files at once
// (LRM 21.3.1, 21.3.2).
module Top;
  int first_mcd;
  int second_mcd;
  int read_fd;

  string first_line;
  string second_line;

  initial begin
    first_mcd = $fopen("fanout_first.txt");
    second_mcd = $fopen("fanout_second.txt");
    $fdisplay(first_mcd | second_mcd, "to both");
    $fclose(first_mcd);
    $fclose(second_mcd);

    read_fd = $fopen("fanout_first.txt", "r");
    void'($fgets(first_line, read_fd));
    $fclose(read_fd);

    read_fd = $fopen("fanout_second.txt", "r");
    void'($fgets(second_line, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (second_mcd === first_mcd)
      $fatal(1, "both opens returned %0h, expected a channel each", first_mcd);
    if (first_line != "to both\n")
      $fatal(1, "the first file held '%s', expected to both", first_line);
    if (second_line != "to both\n")
      $fatal(1, "the second file held '%s', expected to both", second_line);
    $display("All checks passed");
  end
endmodule

// $display ends its output with a newline character and $write does not, so
// text a $write emits stays on its line until a later $display closes it
// (LRM 21.2.1). The file counterparts accept the same arguments after a
// descriptor naming where the output goes (LRM 21.3.2), and $fgets transfers
// a terminating newline into the string it reads (LRM 21.3.4.2), so the
// characters each task emitted are readable back as a value.
module Top;
  int write_fd;
  int read_fd;
  string first_line;
  string trailing;
  int first_count;
  int trailing_count;

  initial begin
    write_fd = $fopen("newline.txt", "w");
    $fwrite(write_fd, "hello");
    $fdisplay(write_fd, " world");
    $fwrite(write_fd, "tail");
    $fclose(write_fd);

    read_fd = $fopen("newline.txt", "r");
    first_count = $fgets(first_line, read_fd);
    trailing_count = $fgets(trailing, read_fd);
    $fclose(read_fd);
  end

  final begin
    if (first_line != "hello world\n")
      $fatal(1, "first line was '%s', expected hello world and a newline",
             first_line);
    if (first_count !== 12)
      $fatal(1, "first line was %0d characters, expected 12", first_count);
    if (trailing != "tail")
      $fatal(1, "trailing text was '%s', expected tail with no newline",
             trailing);
    if (trailing_count !== 4)
      $fatal(1, "trailing text was %0d characters, expected 4",
             trailing_count);
    $display("All checks passed");
  end
endmodule

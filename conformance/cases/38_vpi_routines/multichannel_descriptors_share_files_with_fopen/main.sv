// A multichannel descriptor VPI opens is the kind `$fopen` returns, and the
// two are shared between the system tasks and the VPI routines that take one
// (LRM 38.27): foreign code writes a channel the design opened, the design
// writes one foreign code opened, and opening a file already open answers the
// descriptor it has. One `vpi_mcd_printf` writes every channel its descriptor
// names and answers how many characters it wrote (38.28), and `vpi_mcd_vprintf`
// does the same from a list (38.29); neither writes a file descriptor.
// `vpi_mcd_name` answers the name a channel or a file descriptor was opened
// under (38.26). `vpi_mcd_close` closes every channel it names, and a file
// descriptor `$fopen` opened, for the design too (38.24). What reached each
// file is read back once every one is closed.
module Top;
  import "DPI-C" context function int open_channel(string name);
  import "DPI-C" context function int write_channel(int mcd, string text);
  import "DPI-C" context function int write_channel_from_a_list(int mcd,
                                                                string text);
  import "DPI-C" context function int ends_with_name(int cd, string name);
  import "DPI-C" context function int close_channel(int mcd);
  import "DPI-C" context function int flush_channel(int mcd);

  string first_file = "mcd_first.txt";
  string second_file = "mcd_second.txt";
  string third_file = "mcd_third.txt";
  string fd_file = "mcd_fd.txt";

  int first = 0;
  int second = 0;
  int reopened = 0;
  int third = 0;
  int fd = 0;
  int both_written = -1;
  int listed_written = -1;
  int fd_written = 0;
  int first_named = -1;
  int second_named = -1;
  int fd_named = -1;
  int flushed = -1;
  int two_closed = -1;
  int fd_closed = -1;

  function automatic string contents(string name);
    int file = $fopen(name, "r");
    string all = "";
    string line;
    if (file == 0) return "<not readable>";
    while ($fgets(line, file) != 0) all = {all, line};
    $fclose(file);
    return all;
  endfunction

  initial begin
    first = $fopen(first_file);
    second = open_channel(second_file);
    reopened = open_channel(second_file);
    third = open_channel(third_file);
    fd = $fopen(fd_file, "w");
    first_named = ends_with_name(first, first_file);
    second_named = ends_with_name(second, second_file);
    fd_named = ends_with_name(fd, fd_file);

    $fdisplay(first | second, "from the design");
    both_written = write_channel(first | second, "to both\n");
    listed_written = write_channel_from_a_list(third, "from a list\n");
    fd_written = write_channel(fd, "never written\n");
    $fdisplay(fd, "from the design");
    flushed = flush_channel(first | second | third);

    two_closed = close_channel(first | second);
    $fdisplay(first | second, "after the close");
    $fclose(third);
    fd_closed = close_channel(fd);
  end

  final begin
    if (first == 0 || first[31] !== 1'b0)
      $fatal(1, "$fopen gave %h, not a multichannel descriptor", first);
    if (second == 0 || second[31] !== 1'b0 || second == first)
      $fatal(1, "vpi_mcd_open gave %h, not a channel of its own", second);
    if (reopened !== second)
      $fatal(1, "opening the open file again gave %h, expected %h",
             reopened, second);
    if (first_named !== 1 || second_named !== 1 || fd_named !== 1)
      $fatal(1, "vpi_mcd_name did not name each file it was opened under");
    if (both_written !== 8)
      $fatal(1, "vpi_mcd_printf answered %0d characters, expected 8",
             both_written);
    if (listed_written !== 12)
      $fatal(1, "vpi_mcd_vprintf answered %0d characters, expected 12",
             listed_written);
    if (fd_written !== -1)
      $fatal(1, "vpi_mcd_printf to a file descriptor answered %0d, not EOF",
             fd_written);
    if (flushed !== 0)
      $fatal(1, "vpi_mcd_flush answered %0d, expected 0", flushed);
    if (two_closed !== 0)
      $fatal(1, "closing two channels answered %h, expected 0", two_closed);
    if (fd_closed !== 0)
      $fatal(1, "closing a file descriptor answered %h, expected 0",
             fd_closed);
    if (contents(first_file) != "from the design\nto both\n")
      $fatal(1, "the first file held '%s'", contents(first_file));
    if (contents(second_file) != "from the design\nto both\n")
      $fatal(1, "the second file held '%s'", contents(second_file));
    if (contents(third_file) != "from a list\n")
      $fatal(1, "the third file held '%s'", contents(third_file));
    if (contents(fd_file) != "from the design\n")
      $fatal(1, "the file descriptor's file held '%s'", contents(fd_file));
    $display("All checks passed");
  end
endmodule

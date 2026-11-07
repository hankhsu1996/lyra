// Disabling a named block from inside it continues execution with the
// statement after the block (LRM 9.6.2), whatever runs the code. A static
// variable's initializer runs before any procedure starts (LRM 10.5), and a
// strobed write's arguments are evaluated after every process of the time
// step has run (LRM 21.2.2), so in both a function called there disables a
// block it is inside, and still leaves it.
module Top;
  int write_fd;
  int read_fd;
  string strobed;

  function automatic int leave_early(int value);
    begin : body
      if (value > 1) disable body;
      return 1;
    end
    return 2;
  endfunction

  static int initialized = leave_early(3);

  initial begin
    write_fd = $fopen("disabled_from_a_strobed_argument.txt", "w");
    $fstrobe(write_fd, "%0d", leave_early(3));
    #1 $fclose(write_fd);

    read_fd = $fopen("disabled_from_a_strobed_argument.txt", "r");
    void'($fgets(strobed, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (initialized !== 2)
      $fatal(1, "the initializer's call returned %0d, expected 2",
             initialized);
    if (strobed != "2\n")
      $fatal(1, "the strobed call wrote '%s', expected 2", strobed);
    $display("All checks passed");
  end
endmodule

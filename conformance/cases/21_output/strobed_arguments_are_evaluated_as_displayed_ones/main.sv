// The arguments of a strobed write are specified exactly as those of a display
// task (LRM 21.2.2), so any expression may be one, a function call included,
// and each is evaluated where the write happens, at the end of the time step.
// What such an expression may ask of the run is then what it may ask anywhere:
// a function computes its value from what the step left, a randomization call
// draws a value inside the range it asks for (LRM 18.13.2), and a context
// import is supplied the instantiated scope of its own declaration (LRM
// 35.5.3).
module Top;
  import "DPI-C" context function string where();

  int write_fd;
  int read_fd;
  int count;
  string strobed;

  function automatic int twice(int value);
    return 2 * value;
  endfunction

  initial begin
    write_fd = $fopen("strobed_arguments.txt", "w");
    count = 3;
    $fstrobe(write_fd, "%0d %0d %s", twice(count), $urandom_range(7, 7),
             where());
    count <= 5;
    #1 $fclose(write_fd);

    read_fd = $fopen("strobed_arguments.txt", "r");
    void'($fgets(strobed, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (strobed != "10 7 Top\n")
      $fatal(1, "the strobed write gave '%s', expected '10 7 Top'", strobed);
    $display("All checks passed");
  end
endmodule

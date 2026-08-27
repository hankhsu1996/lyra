// A strobed report is produced at the end of the time step in which the task
// ran -- once every event for that time has happened and just before time
// advances -- so what it reports is each argument's value at that point and
// not the value it held when the task was called. A later assignment in the
// same step, and a nonblocking assignment committing in it, both reach the
// report, which is what separates it from the display task running beside it
// (LRM 21.2.2, 21.3.2).
module Top;
  int write_fd;
  int read_fd;
  int overwritten;
  logic clk;
  int count;

  string strobed_local;
  string displayed_count;
  string strobed_count;

  initial begin
    clk = 0;
    count = 0;
    #1 clk = 1;
  end

  always @(posedge clk) begin
    count <= count + 1;
    $fdisplay(write_fd, "display count=%0d", count);
    $fstrobe(write_fd, "strobe count=%0d", count);
  end

  initial begin
    write_fd = $fopen("strobe_time.txt", "w");
    overwritten = 5;
    $fstrobe(write_fd, "overwritten=%0d", overwritten);
    overwritten = 99;
    #5;
    $fclose(write_fd);

    read_fd = $fopen("strobe_time.txt", "r");
    void'($fgets(strobed_local, read_fd));
    void'($fgets(displayed_count, read_fd));
    void'($fgets(strobed_count, read_fd));
    $fclose(read_fd);
  end

  final begin
    if (strobed_local != "overwritten=99\n")
      $fatal(1, "the strobed local gave '%s', expected its end-of-step 99",
             strobed_local);
    if (displayed_count != "display count=0\n")
      $fatal(1, "the displayed count was '%s', expected 0 before the update",
             displayed_count);
    if (strobed_count != "strobe count=1\n")
      $fatal(1, "the strobed count was '%s', expected 1 after the update",
             strobed_count);
    $display("All checks passed");
  end
endmodule

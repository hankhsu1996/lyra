// A nonblocking assignment carrying an event control reads its right-hand side
// where the statement is reached and schedules the update for the event, without
// suspending the procedure (LRM 9.4.5, 10.4.2). The repeat form waits for that
// many occurrences of the event before the update is made.
module Top;
  logic clk = 0;
  int source = 1;
  int on_edge = 0;
  int after_two = 9;
  int read_before_edge = 9;

  // Rising edges at 5, 15, 25 and 35.
  initial repeat (8) #5 clk = ~clk;

  // Rewriting the operand after time zero reaches neither update.
  initial #1 source = 99;

  initial begin
    on_edge   <= @(posedge clk) source;
    after_two <= repeat (2) @(posedge clk) source;
    // Neither statement blocked, so the procedure is still at time zero and
    // neither update has been made.
    read_before_edge = on_edge;
  end

  final begin
    if (read_before_edge !== 0)
      $fatal(
          1, "the procedure read %0d without blocking, expected the un-updated 0", read_before_edge);
    if (on_edge !== 1)
      $fatal(1, "the edge-scheduled update stored %0d, expected 1", on_edge);
    if (after_two !== 1)
      $fatal(1, "the two-edge-scheduled update stored %0d, expected 1", after_two);
    $display("All checks passed");
  end
endmodule

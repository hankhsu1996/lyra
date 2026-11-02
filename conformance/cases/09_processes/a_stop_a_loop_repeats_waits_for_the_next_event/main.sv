// An event control waits for an event that happens once the process reaches it
// (LRM 9.4.2), so a stop a loop reaches again is not ended by what happened
// while the process was busy elsewhere, whatever it stopped at last time. The
// expression is that of each time the stop is reached: an index that moved
// between two passes names another element (LRM 9.4.2).
module Top;
  bit clk;
  bit [1:0] lines;
  int index;
  time woke[2];
  time line_woke[2];

  initial begin
    #10 clk = 1;
    #10 clk = 0;
    #10 clk = 1;
    #10 clk = 0;
    #10 clk = 1;
  end

  initial begin
    for (int i = 0; i < 2; i = i + 1) begin
      @(posedge clk);
      woke[i] = $time;
      // The clock rises at 30 during this delay, which no stop is waiting at.
      if (i == 0) #25;
    end
  end

  initial begin
    #1 lines[1] = 1;
    #1 lines[0] = 1;
    #1 lines = 0;
    #1 lines[0] = 1;
    #1 lines[1] = 1;
  end

  initial begin
    repeat (2) begin
      @(posedge lines[index]);
      line_woke[index] = $time;
      index = index + 1;
    end
  end

  final begin
    if (woke[0] !== 10) $fatal(1, "the first stop ended at %0t", woke[0]);
    if (woke[1] !== 50)
      $fatal(1, "an edge no stop waited at ended the second stop: %0t", woke[1]);
    if (line_woke[0] !== 2)
      $fatal(1, "the first element's stop ended at %0t", line_woke[0]);
    if (line_woke[1] !== 5)
      $fatal(1, "the second pass stopped at %0t", line_woke[1]);
    $display("All checks passed");
  end
endmodule

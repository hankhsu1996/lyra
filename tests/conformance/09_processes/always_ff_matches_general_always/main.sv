// always_ff is one of the four forms of always procedure and repeats
// continuously like the rest of them (LRM 9.2.2). What the keyword adds is the
// restriction that the procedure hold one and only one event control and no
// blocking timing control, together with the restrictions on assignments to its
// targets by other processes -- not a different rule for when the body runs
// (LRM 9.2.2.4). A general purpose always procedure given the same event
// control and the same body therefore reaches the same value on every edge.
module Top;
  bit clk;
  int general;
  int sequential;

  int general_after_one_edge;
  int sequential_after_one_edge;

  always @(posedge clk) general <= general + 1;
  always_ff @(posedge clk) sequential <= sequential + 1;

  initial begin
    #5 clk = 1;
    #2 general_after_one_edge = general;
    sequential_after_one_edge = sequential;
    #3 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
    #1;
  end

  final begin
    if (general_after_one_edge !== 1)
      $fatal(1, "general_after_one_edge was %0d, expected 1",
             general_after_one_edge);
    if (sequential_after_one_edge !== 1)
      $fatal(1, "sequential_after_one_edge was %0d, expected 1",
             sequential_after_one_edge);
    if (general !== 3) $fatal(1, "general was %0d, expected 3", general);
    if (sequential !== 3)
      $fatal(1, "sequential was %0d, expected 3", sequential);
    $display("All checks passed");
  end
endmodule

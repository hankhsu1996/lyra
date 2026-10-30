// @reports: error
//
// Scheduling what cannot become active until a function returns is allowed
// only on a thread an initial or always procedure created, or a fork of one,
// and a tool issues an error otherwise (LRM 13.4.4). A concurrent assertion's
// pass statement runs on neither, so a nonblocking assignment made by a
// function it calls is an error, though the same assignment written in the
// pass statement itself would not be.
module Top;
  bit clk;
  int x;

  function void schedule();
    x <= 1;
  endfunction

  assert property (@(posedge clk) 1) schedule();

  initial begin
    #1;
    $display("Ready for the report");
    clk = 1;
    #1;
    $display("All checks passed");
  end
endmodule

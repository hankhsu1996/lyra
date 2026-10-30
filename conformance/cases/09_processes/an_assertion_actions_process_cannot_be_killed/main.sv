// @reports: error
//
// kill(), await(), suspend() and resume() are restricted to a process created
// by an initial or always procedure, or a fork of one (LRM 9.7). A concurrent
// assertion's pass statement is neither, so killing the process it ran on is an
// error even from an initial procedure, whose own process may be controlled.
module Top;
  bit clk;
  process action_process;

  assert property (@(posedge clk) 1) action_process = process::self();

  initial begin
    #1 clk = 1;
    #1;
    $display("Ready for the report");
    action_process.kill();
    $display("All checks passed");
  end
endmodule

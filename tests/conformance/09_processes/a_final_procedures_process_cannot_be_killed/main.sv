// @reports: error
//
// kill(), await(), suspend() and resume() are restricted to a process created
// by an initial or always procedure, or a fork of one (LRM 9.7). The final
// procedures run as function calls from a process neither created (LRM 9.2.3),
// so killing it from inside one is an error.
module Top;
  initial #5;

  final begin
    process self_process;
    self_process = process::self();
    $display("Ready for the report");
    self_process.kill();
    $display("All checks passed");
  end
endmodule

// @error: applies only to a process an initial or always procedure created
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
    self_process.kill();
  end
endmodule

// $system passes its argument to the C function system(), which executes it as
// if it had been typed at the terminal, and may be called either as a task or
// as a function; called as a function it yields what system() returned, typed
// int, and called with no argument it reaches system() with the null string
// (LRM 20.17.1). What that answer means beyond success or failure belongs to
// the host and not to the standard, so the checks below read only the
// distinction C fixes and never a particular encoding of it. Telling a command
// that succeeded from one that failed is also what shows the argument reached a
// command processor at all, which the checks presume the host has.
module Top;
  string command;
  int success;
  int failure;
  int computed;
  int processor;
  int progress;

  initial begin
    success = $system("exit 0");
    failure = $system("exit 1");
    command = "exit 0";
    computed = $system(command);
    processor = $system;

    progress = 1;
    $system("exit 0");
    progress = 2;
    $system("exit 1");
    progress = 3;
    $system;
    progress = 4;
  end

  final begin
    if (success !== 0)
      $fatal(1, "a command that succeeded reported %0d, expected 0", success);
    if (failure === 0)
      $fatal(1, "a command that failed reported 0, expected a nonzero status");
    if (computed !== 0)
      $fatal(1, "a command read from a variable reported %0d, expected 0",
             computed);
    if (processor === 0)
      $fatal(1, "the null command reported 0, expected a nonzero status");
    if (progress !== 4)
      $fatal(1, "the task form reached %0d, expected 4", progress);
    $display("All checks passed");
  end
endmodule

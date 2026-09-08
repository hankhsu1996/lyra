// @reports: assumption failed
//
// A deferred immediate assume carries the same simulation behavior as a
// deferred assert: its expression is evaluated where the statement is reached,
// and a false one with no fail statement reaches the tool's own report, held to
// a later region of the time step (LRM 16.4). The difference from assert is
// verification intent for a formal tool, not what simulation does, which is why
// the report names an assumption rather than an assertion. The report is the
// whole observable, so the program itself checks only that the procedure
// carried on past the failure.
module Top;
  int completed;

  initial begin
    completed = 0;
    assume #0 (0);
    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed deferred assumption stopped its procedure");
    $display("All checks passed");
  end
endmodule

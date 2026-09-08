// @reports: assertion failed
//
// A final deferred immediate assertion is a deferred assertion whose action is
// held to the Postponed region rather than the Reactive one (LRM 16.4): the
// expression is still evaluated where the statement is reached, and a false one
// with no fail statement still reaches the tool's own report. What is claimed
// here is that the `final` form reaches that report at all; the report is the
// whole observable, so the program itself checks only that the procedure
// carried on past the failure.
module Top;
  int completed;

  initial begin
    completed = 0;
    assert final (0);
    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed final deferred assertion stopped its procedure");
    $display("All checks passed");
  end
endmodule

// @reports: final saw 2
//
// A final deferred assertion schedules its fail statement in the Postponed
// region (LRM 16.4), and the statement may be a system task. Its arguments are
// evaluated where the assertion is, so the report names the value the variable
// held then, not the one it holds when the time step ends.
module Top;
  int v;
  int completed;

  initial begin
    completed = 0;
    v = 2;
    assert final (v == 0) else $error("final saw %0d", v);
    v = 3;
    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed final deferred assertion stopped its procedure");
    $display("All checks passed");
  end
endmodule

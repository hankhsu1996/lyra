// @reports: assertion failed
//
// A failed immediate assert with no fail statement is reported by the tool at
// error severity, and a pass statement does not take the place of that report:
// the pass statement is what a true expression selects, so a false one still
// reaches the default action (LRM 16.3). A report is the whole observable, so
// what the program itself checks is that neither pass statement ran and that
// the procedure carried on past both failures.
module Top;
  int completed;

  initial begin
    completed = 0;

    assert (0);

    assert (0) $fatal(1, "the pass statement ran on a false assertion");

    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed assertion stopped the procedure that reached it");
    $display("All checks passed");
  end
endmodule

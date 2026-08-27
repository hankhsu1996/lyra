// $info, $warning and $error each generate a run-time message of their
// severity and leave the simulation running; of the severity system tasks only
// $fatal terminates it, by an implicit call to $finish (LRM 20.10). Reporting
// is the whole of what the other three do, so a call changes no value and no
// control flow however many times it is made. The user-defined message is
// optional and uses $display syntax, so a call may carry no arguments, a
// format string alone, or a format string with any number of arguments after
// it.
module Top;
  int progress;
  int reports;

  initial begin
    $info;
    progress = 1;
    $info("an informational message");
    progress = 2;
    $warning("a warning carrying %0d and %s", 42, "text");
    progress = 3;
    $error("an error carrying %0d", 7);
    progress = 4;
    for (int i = 0; i < 12; i = i + 1) begin
      $warning("a repeated warning, occurrence %0d", i);
      reports = reports + 1;
    end
    progress = 5;
  end

  final begin
    if (progress !== 5)
      $fatal(1, "progress reached %0d, expected 5", progress);
    if (reports !== 12)
      $fatal(1, "the loop made %0d reports, expected 12", reports);
    $display("All checks passed");
  end
endmodule

// @reports: assertion failed
//
// A failed deferred immediate assert with no fail statement reaches the tool's
// own report, held back to the Reactive region rather than issued where the
// statement is reached (LRM 16.4). A pass statement does not take the place of
// that report. The procedure carries on past the failure, so what the program
// checks in SystemVerilog is that it completed; the report itself is the
// observable stated above.
module Top;
  int completed;

  initial begin
    completed = 0;
    assert #0 (0);
    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a failed deferred assertion stopped its procedure");
    $display("All checks passed");
  end
endmodule

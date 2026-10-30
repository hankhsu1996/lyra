// @reports: error line=11
//
// A failed deferred immediate assert with no fail statement reaches the tool's
// own report, held back to the Reactive region rather than issued where the
// statement is reached (LRM 16.4). A pass statement does not take the place of
// that report. The report is the observable stated above; the sentinel a time
// step later says the run went on past it.
module Top;
  initial begin
    $display("Ready for the report");
    assert #0 (0);
    #1 $display("All checks passed");
  end
endmodule

// @reports: error line=12
//
// A final deferred immediate assertion is a deferred assertion whose action is
// held to the Postponed region rather than the Reactive one (LRM 16.4): the
// expression is still evaluated where the statement is reached, and a false one
// with no fail statement still reaches the tool's own report. What is claimed
// here is that the `final` form reaches that report at all; the sentinel a time
// step later says the run went on past it.
module Top;
  initial begin
    $display("Ready for the report");
    assert final (0);
    #1 $display("All checks passed");
  end
endmodule

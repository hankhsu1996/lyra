// @reports: error line=11 message="an error carrying 7 and text"
//
// $error generates a run-time error, and its user-defined message uses
// $display syntax, so the report carries the text its arguments format to
// (LRM 20.10). Nothing in the standard promises the run goes on after an
// error, so the case is ready for the report before the call and passes only
// where the run reaches the statement after it.
module Top;
  initial begin
    $display("Ready for the report");
    $error("an error carrying %0d and %s", 7, "text");
    $display("All checks passed");
  end
endmodule

// @reports: error line=13
// @reports: error line=15
//
// A failed immediate assert with no fail statement is reported by the tool at
// error severity, and a pass statement does not take the place of that report:
// the pass statement is what a true expression selects, so a false one still
// reaches the default action (LRM 16.3). A report is the whole observable, so
// what the program itself checks is that neither pass statement ran, and the
// sentinel after both says the run went on past them.
module Top;
  initial begin
    $display("Ready for the report");
    assert (0);

    assert (0) $fatal(1, "the pass statement ran on a false assertion");

    $display("All checks passed");
  end
endmodule

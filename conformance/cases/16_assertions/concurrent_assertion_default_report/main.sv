// @reports: error line=20 count=3
//
// A concurrent assert whose action block carries no else clause is reported by
// the tool when the property is false, at error severity, and a pass statement
// does not take the place of that report (LRM 16.14.1). The report is the whole
// observable of that requirement, so what the program itself checks is the
// other half: that the pass statement ran for the attempts that held, and,
// where the run goes on past the failing ones, for no others.
module Top;
  logic clk = 0;
  logic ok = 1;

  int ticks = 0;
  int passes = 0;

  always #5 clk = ~clk;

  always @(posedge clk) ticks = ticks + 1;

  a_default: assert property (@(posedge clk) ok) passes = passes + 1;

  // Ticks land at 5, 15, 25, 35 and 45, and `ok` is sampled high at the first
  // two of them, so the attempts at the last three fail and report.
  initial begin
    #18;
    if (passes !== 2)
      $fatal(1, "the pass statement ran for %0d attempts, expected 2", passes);
    $display("Ready for the report");
    ok = 0;
    #30;
    if (ticks !== 5) $fatal(1, "the clock reached %0d ticks, expected 5",
                            ticks);
    if (passes !== 2)
      $fatal(1, "the pass statement ran for %0d attempts, expected 2", passes);
    $display("All checks passed");
    $finish;
  end
endmodule

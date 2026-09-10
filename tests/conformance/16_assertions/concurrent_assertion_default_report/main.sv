// @reports: assertion failed
//
// A concurrent assert whose action block carries no else clause is reported by
// the tool when the property is false, at error severity, and a pass statement
// does not take the place of that report (LRM 16.14.1). The report is the whole
// observable of that requirement, so what the program itself checks is the
// other half: that the pass statement ran for the attempts that held and for no
// others, and that a failing attempt stopped nothing.
module Top;
  logic clk = 0;
  logic ok = 1;

  int ticks = 0;
  int passes = 0;

  always #5 clk = ~clk;

  always @(posedge clk) ticks = ticks + 1;

  a_default: assert property (@(posedge clk) ok) passes = passes + 1;

  // Ticks land at 5, 15, 25, 35 and 45, and `ok` is sampled high at the first
  // two of them.
  initial begin
    #18 ok = 0;
    #30 $finish;
  end

  final begin
    if (ticks !== 5) $fatal(1, "the clock reached %0d ticks, expected 5",
                            ticks);
    if (passes !== 2)
      $fatal(1, "the pass statement ran for %0d attempts, expected 2", passes);
    $display("All checks passed");
  end
endmodule

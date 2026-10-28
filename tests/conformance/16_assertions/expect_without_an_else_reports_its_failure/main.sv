// @reports: error line=17
//
// An expect with no else clause takes the failure of its property to the
// default action, the tool's own `$error` (LRM 16.17, 16.14.1), and the process
// goes on with the statement after it. The property here fails at the first
// tick that cannot extend it, and where the run goes on past the report, the
// statement after the expect runs at that tick.
module Top;
  logic clk = 0;
  logic c = 1;

  always #5 clk = ~clk;

  initial begin
    #2;
    $display("Ready for the report");
    expect (@(posedge clk) c ##1 c);
    if ($time !== 15)
      $fatal(1, "the expect went on at %0t, expected 15", $time);
    $display("All checks passed");
    $finish;
  end

  initial #9 c = 0;
endmodule

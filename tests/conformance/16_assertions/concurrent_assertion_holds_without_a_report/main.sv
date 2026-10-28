// @reports-none: error
//
// The default report belongs to a false result alone. A concurrent assert with
// no action block at all writes nothing for an attempt that holds (LRM
// 16.14.1), so a design whose properties are all true is one a conforming tool
// reports no failure for -- however many attempts it evaluated. A failing one whose
// else clause is a null statement has none either: "When no action is needed,
// a null statement (i.e., ;) is specified", and the tool's report stands only
// where the else clause is omitted (LRM 16.14.1).
//
// A program cannot read a message about itself, so what it checks here is that
// the attempts happened: the clock ticked, and the procedure that counts its
// ticks ran beside an assertion that never reports.
module Top;
  logic clk = 0;
  logic ok = 0;

  int ticks = 0;

  initial repeat (10) #5 clk = ~clk;

  always @(posedge clk) ticks = ticks + 1;

  a_silent: assert property (@(posedge clk) ok || !ok);
  a_null_else: assert property (@(posedge clk) ok) else ;

  final begin
    if (ticks !== 5) $fatal(1, "the clock reached %0d ticks, expected 5",
                            ticks);
    $display("All checks passed");
  end
endmodule

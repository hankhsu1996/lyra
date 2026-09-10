// @reports-nothing:
//
// The default report belongs to a false result alone. A concurrent assert with
// no action block at all writes nothing for an attempt that holds (LRM
// 16.14.1), so a design whose properties are all true is one a conforming tool
// has no comment on -- however many attempts it evaluated.
//
// A program cannot read a message about itself, so what it checks here is that
// the attempts happened: the clock ticked, and the procedure that counts its
// ticks ran beside an assertion that never reports.
module Top;
  logic clk = 0;
  logic ok = 0;

  int ticks = 0;

  always #5 clk = ~clk;

  always @(posedge clk) ticks = ticks + 1;

  a_silent: assert property (@(posedge clk) ok || !ok);

  initial #48 $finish;

  final begin
    if (ticks !== 5) $fatal(1, "the clock reached %0d ticks, expected 5",
                            ticks);
    $display("All checks passed");
  end
endmodule

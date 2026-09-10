// @reports-nothing:
//
// A `restrict` states a constraint for a formal tool to converge a proof on,
// and it is the one assertion directive a simulator does not verify: it has the
// semantics of `assume property`, except that it is not checked and carries no
// action block at all (LRM 16.14.4). So a run over a design carrying one
// behaves exactly as a run over the same design with it deleted, however false
// the property it states is -- which is what this case pins, by restricting a
// property that is false at every tick of its clock.
//
// A program cannot read a message about itself, so what it checks is that the
// run happened: the clock ticked, and the procedure counting its ticks ran
// beside a directive that reaches no report and runs no statement.
module Top;
  logic clk = 0;
  logic never_true = 0;

  int ticks = 0;

  initial repeat (10) #5 clk = ~clk;

  always @(posedge clk) ticks = ticks + 1;

  restrict property (@(posedge clk) never_true);

  final begin
    if (ticks !== 5) $fatal(1, "the clock reached %0d ticks, expected 5",
                            ticks);
    $display("All checks passed");
  end
endmodule

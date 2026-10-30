// A general purpose always procedure is only useful with some form of timing
// control, because its looping nature otherwise leaves simulation time unable
// to advance (LRM 9.2.2.1). A delay control gives it one: the statement is
// delayed by the named amount with respect to the statement before it, so an
// always procedure that inverts a variable under a delay control repeats that
// inversion once per delay period for as long as the simulation runs (LRM
// 9.4.1).
module Top;
  bit clk;
  bit at_2;
  bit at_7;
  bit at_12;
  bit at_17;
  bit at_22;

  always #5 clk = ~clk;

  initial begin
    #2 at_2 = clk;
    #5 at_7 = clk;
    #5 at_12 = clk;
    #5 at_17 = clk;
    #5 at_22 = clk;
    $finish;
  end

  final begin
    if (at_2 !== 1'b0) $fatal(1, "at_2 was %b, expected 0", at_2);
    if (at_7 !== 1'b1) $fatal(1, "at_7 was %b, expected 1", at_7);
    if (at_12 !== 1'b0) $fatal(1, "at_12 was %b, expected 0", at_12);
    if (at_17 !== 1'b1) $fatal(1, "at_17 was %b, expected 1", at_17);
    if (at_22 !== 1'b0) $fatal(1, "at_22 was %b, expected 0", at_22);
    $display("All checks passed");
  end
endmodule

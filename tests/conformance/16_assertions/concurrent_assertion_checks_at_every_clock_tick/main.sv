// A concurrent assertion written outside procedural code has always semantics:
// a new evaluation attempt of its property begins at every occurrence of the
// leading clock event, and nothing else starts one (LRM 16.14.5). Each attempt
// carries its own result, and that result selects which arm of the action block
// runs -- the pass statements on true, the fail statements on false (LRM
// 16.14.1). Both run in the Reactive region, after the regions in which the
// time step settles the values the attempt read.
//
// What the attempt reads is the sampled value of the expression, taken as of
// the Preponed region of the tick's own time step (LRM 16.5, 16.5.1), so a
// write that lands between two ticks is seen whole by the later one and not at
// all by the earlier.
module Top;
  logic clk = 0;
  logic ok = 1;

  int passes = 0;
  int fails = 0;

  always #5 clk = ~clk;

  a_every_tick: assert property (@(posedge clk) ok)
      passes = passes + 1;
    else
      fails = fails + 1;

  // Ticks land at 5, 15, 25, 35 and 45. Both writes happen between two of
  // them, so every attempt reads a settled value: 1, 1, 0, 0, 1.
  initial begin
    #18 ok = 0;
    #20 ok = 1;
    #10 $finish;
  end

  final begin
    if (passes !== 3) $fatal(1, "the property held at %0d ticks, expected 3",
                             passes);
    if (fails !== 2) $fatal(1, "the property failed at %0d ticks, expected 2",
                            fails);
    $display("All checks passed");
  end
endmodule

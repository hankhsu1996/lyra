// The sampled value of a constant cast is the current value of its argument,
// where every other expression in an assertion reads the value its variables
// held in the Preponed region (LRM 16.5.1). So where a variable changes in the
// same time step as the clock's edge, before the Observed region, a cast of it
// reads the new value and a plain read the old one.
module Top;
  logic clk = 0;
  logic a = 0;

  int cast_fails = 0;
  int plain_fails = 0;

  initial begin
    #10 clk = 1;
    a = 1;
  end

  as_now: assert property (@(posedge clk) const'(a)) else cast_fails++;
  as_sampled: assert property (@(posedge clk) a) else plain_fails++;

  initial #15 $finish;

  final begin
    if (cast_fails !== 0)
      $fatal(1, "the cast read the old value %0d times", cast_fails);
    if (plain_fails !== 1)
      $fatal(1, "the plain read failed %0d times, expected 1", plain_fails);
    $display("All checks passed");
  end
endmodule

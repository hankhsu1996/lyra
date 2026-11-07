// Where a sampled value function asks for a past value of a constant cast, the
// cast's current value is taken instead (LRM 16.5.1): a constant cast has no
// history, so `$past(const'(a))` is what `a` is now, whatever it was at the
// tick before.
module Top;
  logic clk = 0;
  logic [3:0] a = 4'd1;

  int fails = 0;

  always #5 clk = ~clk;

  // `a` steps at every falling edge, so at each rising edge after the first
  // its value differs from the one it had a tick earlier.
  always @(negedge clk) a = a + 4'd1;

  now: assert property (@(posedge clk) $past(const'(a)) == const'(a))
    else fails++;

  initial #40 $finish;

  final begin
    if (fails !== 0)
      $fatal(1, "a past value of the cast differed from it %0d times", fails);
    $display("All checks passed");
  end
endmodule

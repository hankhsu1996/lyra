// @top: Top
//
// An attempt still in flight when the run ends comes to what the finished
// trace says of it (Annex F.5.3.1), and where that fails, the assertion's
// action runs then. No thread creates the action (LRM 16.14.1) and no tick
// decides it, so nothing hands a generator over to it, and the standard says
// nothing more of where one comes from. Lyra gives it the next generator of
// its instance's initialization RNG, as a static process takes one (LRM
// 18.14.1), the actions of an instance running in the order their assertions
// registered.
//
// So what ran before the end does not move it: a strobed write's arguments run
// on no thread once their region runs them (LRM 21.2.2), and the effect
// running them takes back what they ran in as it returns, so nothing of it is
// left to hand the action anything. Each instance of `Ends` starts from the
// same seeds as every other (LRM 18.14.1), so the action's draw is the same in
// an instance that strobed as in one that did not.
module Ends #(
    parameter bit Strobes = 0
);
  logic clk = 0;
  logic a = 1;
  logic b = 1;
  int unsigned end_draw = 0;
  int fails = 0;

  always #5 clk = ~clk;

  initial begin
    if (Strobes) $strobe("strobed %0d", $urandom);
  end

  // Ticks land at 5, 15 and 25, and the run ends at 30, so the attempt of the
  // last tick is still waiting for `b` and, being strong, fails then.
  a_ends: assert property (@(posedge clk) a |-> strong(##1 b))
    else begin
      end_draw = $urandom;
      fails++;
    end
endmodule

module Top;
  Ends #(.Strobes(0)) quiet ();
  Ends #(.Strobes(1)) strobing ();

  initial #30 $finish;

  final begin
    if (quiet.fails !== 1 || strobing.fails !== 1)
      $fatal(1, "the assertions failed %0d and %0d times, expected once each",
             quiet.fails, strobing.fails);
    if (quiet.end_draw !== strobing.end_draw)
      $fatal(1, "the action the end of the run started drew %0d, and %0d where a strobe had run",
             quiet.end_draw, strobing.end_draw);
    $display("All checks passed");
  end
endmodule

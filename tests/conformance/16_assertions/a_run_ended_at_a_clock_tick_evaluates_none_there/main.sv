// `$finish` makes the simulator exit where it is called (LRM 20.2), so a clock
// tick of that same time step reaches no Observed region and no
// concurrent assertion is evaluated there (LRM 16.5). An attempt that would
// have begun at that tick never begins, and one that would have failed there
// is judged by what the trace up to the previous tick says (Annex F.5.3.1).
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;

  int starts_passes = 0, starts_fails = 0;
  int waits_passes = 0, waits_fails = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15 and 25, and the run ends at 25. `a` is sampled high
  // at the tick at 15 and would be read high at 25; `b` is low throughout.
  initial begin
    #8 a = 1;
    #17 $finish;
  end

  // Read at 5 and 15 only, so its one matching attempt, at 15, fails there;
  // the one at 25 would have failed too.
  a_starts: assert property (@(posedge clk) a |-> b)
      starts_passes++;
    else
      starts_fails++;
  // The attempt at 15 would read `b` at 25, where it is low, and is instead
  // still waiting when the run ends, where a weak sequence holds (Annex
  // F.5.3.1); whether its pass action runs then the standard does not say, but
  // it never fails.
  a_waits: assert property (@(posedge clk) a |=> b)
      waits_passes++;
    else
      waits_fails++;

  final begin
    if (starts_passes !== 1 || starts_fails !== 1)
      $fatal(1, "a_starts held %0d and failed %0d times, expected 1 and 1",
             starts_passes, starts_fails);
    if (!(waits_passes inside {1, 2}) || waits_fails !== 0)
      $fatal(1, "a_waits held %0d and failed %0d times, expected 1 or 2 and 0",
             waits_passes, waits_fails);
    $display("All checks passed");
  end
endmodule

// An implication makes the check of its consequent conditional on a match of
// its antecedent (LRM 16.12.7). The two forms differ only in where the
// consequent starts: for the overlapped form `|->` the end point of the
// antecedent's match is the start point of the consequent, and for the
// nonoverlapped form `|=>` the consequent starts at the tick after it.
//
// The other half of the operator is what happens when the antecedent does not
// match at all: evaluation of the implication succeeds and returns true, so the
// attempt is true and the pass statements run (LRM 16.12.7, 16.14.1). That is
// why both counters are checked here -- a tool that simply ignored an attempt
// whose antecedent failed would leave the fail counts right and the pass counts
// short.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;

  int overlap_passes = 0;
  int overlap_fails = 0;
  int next_passes = 0;
  int next_fails = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25, 35 and 45. Both `a` and `b` are sampled high at
  // the tick at 15 and low at every other, so the antecedent matches once.
  initial begin
    #8;
    a = 1;
    b = 1;
    #10;
    a = 0;
    b = 0;
    #30 $finish;
  end

  a_overlap: assert property (@(posedge clk) a |-> b)
      overlap_passes = overlap_passes + 1;
    else
      overlap_fails = overlap_fails + 1;

  a_next: assert property (@(posedge clk) a |=> b)
      next_passes = next_passes + 1;
    else
      next_fails = next_fails + 1;

  final begin
    // Five attempts: one matches the antecedent and reads `b` at that same
    // tick, and the four whose antecedent is false are true without reading it.
    if (overlap_passes !== 5)
      $fatal(1, "the overlapped form held at %0d attempts, expected 5",
             overlap_passes);
    if (overlap_fails !== 0)
      $fatal(1, "the overlapped form failed %0d times, expected none",
             overlap_fails);
    // The one matching attempt reads `b` a tick later, where it is low.
    if (next_passes !== 4)
      $fatal(1, "the nonoverlapped form held at %0d attempts, expected 4",
             next_passes);
    if (next_fails !== 1)
      $fatal(1, "the nonoverlapped form failed %0d times, expected 1",
             next_fails);
    $display("All checks passed");
  end
endmodule

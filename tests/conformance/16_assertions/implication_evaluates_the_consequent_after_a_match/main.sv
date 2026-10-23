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
//
// An antecedent may match more than once in one attempt, and the consequent is
// then evaluated once for each match; the implication holds only where every
// one of those evaluations does (LRM 16.12.7). A consequent may read a sampled
// value function, whose history is that of every tick of the clock, whether or
// not the attempt read anything there (LRM 16.9.3).
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

  // Ticks k = 0..4 land at 10k + 5, and these are written at 10k, so each tick
  // reads them sampled. `t` holds at k = 0 only, so the attempt that began
  // there matches `t ##[1:2] u` at k = 1 and at k = 2, and reads `v` a tick
  // after each: low at k = 2 and high at k = 3. The first evaluation is still
  // live when the second starts, so the two only answer apart if they are
  // kept apart.
  logic t = 0;
  logic u = 0;
  logic v = 0;
  logic w = 0;
  localparam logic [0:4] T = 5'b10000;
  localparam logic [0:4] U = 5'b01100;
  localparam logic [0:4] V = 5'b00010;
  localparam logic [0:4] W = 5'b01000;

  initial begin
    for (int k = 0; k < 5; k++) begin
      t = T[k];
      u = U[k];
      v = V[k];
      w = W[k];
      #10;
    end
  end

  int every_passes = 0;
  int every_fails = 0;
  int past_two_passes = 0;
  int past_two_fails = 0;
  int past_one_passes = 0;
  int past_one_fails = 0;

  a_every_match: assert property (@(posedge clk) t ##[1:2] u |-> ##1 v)
      every_passes = every_passes + 1;
    else
      every_fails = every_fails + 1;

  // Read at k = 3, three ticks after `t`: two ticks before is k = 1, where `w`
  // was high, and one tick before is k = 2, where it was low.
  a_past_two: assert property (@(posedge clk) t |-> ##3 $past(w, 2))
      past_two_passes = past_two_passes + 1;
    else
      past_two_fails = past_two_fails + 1;
  a_past_one: assert property (@(posedge clk) t |-> ##3 $past(w))
      past_one_passes = past_one_passes + 1;
    else
      past_one_fails = past_one_fails + 1;

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
    // The attempt at k = 0 fails at k = 2, though its second match holds; the
    // other four never match the antecedent.
    if (every_passes !== 4 || every_fails !== 1)
      $fatal(1, "with two matches, %0d held and %0d failed, expected 4 and 1",
             every_passes, every_fails);
    if (past_two_passes !== 5 || past_two_fails !== 0)
      $fatal(1, "$past(w, 2) held %0d and failed %0d times, expected 5 and 0",
             past_two_passes, past_two_fails);
    if (past_one_passes !== 4 || past_one_fails !== 1)
      $fatal(1, "$past(w) held %0d and failed %0d times, expected 4 and 1",
             past_one_passes, past_one_fails);
    $display("All checks passed");
  end
endmodule

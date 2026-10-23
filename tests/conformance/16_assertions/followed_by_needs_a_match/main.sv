// A followed-by property holds only where its antecedent matches from the
// start point and the consequent holds from one of those matches: at the
// match's end point for `#-#`, at the tick after it for `#=#` (LRM 16.12.9).
// It is the dual of implication -- `s #-# p` is `not (s |-> not p)` -- so an
// antecedent that never matches makes it false where it makes an implication
// true, and one match among several is enough.
//
// A nonoverlapped consequent starts at the tick after the match, which is the
// last tick of the antecedent `s ##1 1` (Annex F.3.4.3.3). Where the run ends
// before that tick, the antecedent has not matched, so the followed-by is
// false and the implication true however strong the consequent is.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  logic c = 0;
  logic d = 0;

  always #5 clk = ~clk;

  // Ticks k = 0..5 land at 10k + 5, and each value is written at 10k, so the
  // tick reads it sampled. The run ends at 60, before a seventh tick.
  localparam logic [0:5] A = 6'b101101;
  localparam logic [0:5] B = 6'b000100;
  localparam logic [0:5] C = 6'b110100;
  localparam logic [0:5] D = 6'b010010;

  initial begin
    for (int k = 0; k < 6; k++) begin
      a = A[k];
      b = B[k];
      c = C[k];
      d = D[k];
      #10;
    end
  end

  initial #60 $finish;

  int overlap_pass = 0, overlap_fail = 0;
  int next_pass = 0, next_fail = 0;
  int strong_pass = 0, strong_fail = 0;
  int same_tick = 0;
  int tick_after = 0;
  int any_match = 0;

  a_overlap: assert property (@(posedge clk) a #-# (c ##1 d))
      overlap_pass++;
    else
      overlap_fail++;
  a_next: assert property (@(posedge clk) a #=# c)
      next_pass++;
    else
      next_fail++;
  a_strong: assert property (@(posedge clk) a |=> strong(c))
      strong_pass++;
    else
      strong_fail++;
  // `a ##1 b` matches once, ending at k = 3, where `c` is high and after
  // which it is low.
  c_same_tick: cover property (@(posedge clk) (a ##1 b) #-# c) same_tick++;
  c_tick_after: cover property (@(posedge clk) (a ##1 b) #=# c) tick_after++;
  // From k = 0 and k = 1 one of the two matches of the antecedent is followed
  // by `a`, and from k = 3 the later one is.
  c_any_match: cover property (@(posedge clk) (c ##[0:1] d) #=# a)
      any_match++;

  function automatic void check(string name, int passes, int fails,
                                int want_passes, int want_fails);
    if (passes !== want_passes || fails !== want_fails)
      $fatal(1, "%s held %0d and failed %0d times, expected %0d and %0d",
             name, passes, fails, want_passes, want_fails);
  endfunction

  final begin
    check("#-#", overlap_pass, overlap_fail, 2, 4);
    // The attempt at k = 5 ends before its consequent's tick, which fails it.
    check("#=#", next_pass, next_fail, 2, 4);
    // The same attempt holds vacuously for the implication, as do those
    // at k = 1 and k = 4, where `a` is low.
    check("|=> strong", strong_pass, strong_fail, 5, 1);
    if (same_tick !== 1)
      $fatal(1, "#-# was covered %0d times, expected 1", same_tick);
    if (tick_after !== 0)
      $fatal(1, "#=# was covered %0d times, expected none", tick_after);
    if (any_match !== 3)
      $fatal(1, "#=# after a range was covered %0d times, expected 3",
             any_match);
    $display("All checks passed");
  end
endmodule

// What an attempt still in flight when the run ends comes to is what the trace
// up to then says (Annex F.5.3.1), and that depends on the strength of each
// sequence in it, not on the statement (LRM 16.12.2). A weak sequence holds
// on a trace that has not shown it unable to match, so an assert whose
// sequence could still match holds and a cover written with `weak` is
// covered. A strong sequence holds only where it matched, so an assert written
// with `strong` fails and a cover's sequence, strong unless written otherwise,
// is not covered. `a |=> p` is `(a ##1 1) |-> p` (Annex F.3.4.3.3), so an `a`
// at the last tick leaves an antecedent that never matched, and the attempt
// holds whatever the strength of `p`.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;

  always #5 clk = ~clk;

  // Ticks k = 0..5 land at 10k + 5, and each value is written at 10k, so the
  // tick reads it sampled. The run ends at 60, before a seventh tick, so an
  // attempt at k = 5 whose `a` holds is still waiting for `b`.
  localparam logic [0:5] A = 6'b101101;
  localparam logic [0:5] B = 6'b000100;

  initial begin
    for (int k = 0; k < 6; k++) begin
      a = A[k];
      b = B[k];
      #10;
    end
  end

  initial #60 $finish;

  int strong_pass = 0, strong_fail = 0;
  int weak_pass = 0, weak_fail = 0;
  int weak_covered = 0;
  int strong_covered = 0;

  a_strong: assert property (@(posedge clk) a |-> strong(a ##1 b))
      strong_pass++;
    else
      strong_fail++;
  a_weak: assert property (@(posedge clk) a |-> (a ##1 b))
      weak_pass++;
    else
      weak_fail++;
  int next_pass = 0, next_fail = 0;
  a_next_strong: assert property (@(posedge clk) a |=> strong(b))
      next_pass++;
    else
      next_fail++;
  c_weak: cover property (@(posedge clk) weak(a ##1 b)) weak_covered++;
  c_strong: cover property (@(posedge clk) a ##1 b) strong_covered++;

  function automatic void check(string name, int passes, int fails,
                                int want_passes, int want_fails);
    if (passes !== want_passes || fails !== want_fails)
      $fatal(1, "%s held %0d and failed %0d times, expected %0d and %0d",
             name, passes, fails, want_passes, want_fails);
  endfunction

  final begin
    // `a ##1 b` matches from k = 2 only; `a` is low at k = 1 and k = 4.
    check("a strong sequence", strong_pass, strong_fail, 3, 3);
    check("a weak sequence", weak_pass, weak_fail, 4, 2);
    // `b` is low after `a` at k = 0 and k = 3; the attempt at k = 5 is still
    // in flight and holds.
    check("a strong consequent", next_pass, next_fail, 4, 2);
    if (weak_covered !== 2)
      $fatal(1, "the weak sequence was covered %0d times, expected 2",
             weak_covered);
    if (strong_covered !== 1)
      $fatal(1, "the strong sequence was covered %0d times, expected 1",
             strong_covered);
    $display("All checks passed");
  end
endmodule

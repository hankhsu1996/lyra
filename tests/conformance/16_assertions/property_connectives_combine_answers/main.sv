// A property built from other properties evaluates each operand from the
// attempt's start and combines their results: `not` gives the opposite of its
// operand's, `or` holds where either holds, `and` where both do (LRM 16.12.3,
// 16.12.4, 16.12.5), `p1 implies p2` where p1 is false or p2 is true, and
// `p1 iff p2` where both agree (LRM 16.12.8). An attempt answers as soon as
// its operands settle it, so an `or` holds at the first operand's match and an
// `and` fails at the first operand's failure, and an operand may itself be an
// implication evaluated once per match of its antecedent (LRM 16.12.7).
//
// `not` switches the strength of its operand (LRM 16.12.3). A sequence in an
// assert is weak, so where the run ends with `a ##1 b` still able to match,
// the weak sequence holds and its negation fails -- the example the clause
// itself gives. A sequence in a cover is strong, and where the run ends with
// it still able to match, its negation is covered (LRM 16.12.2, Annex F.5.3.1).
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

  int not_pass = 0, not_fail = 0;
  int or_pass = 0, or_fail = 0;
  int and_pass = 0, and_fail = 0;
  int implies_pass = 0, implies_fail = 0;
  int iff_pass = 0, iff_fail = 0;
  int consequent_pass = 0, consequent_fail = 0;
  int operands_pass = 0, operands_fail = 0;
  int negated_pass = 0, negated_fail = 0;
  int covered = 0;

  a_not: assert property (@(posedge clk) not (a ##1 b))
      not_pass++;
    else
      not_fail++;
  a_or: assert property (@(posedge clk) (a ##1 b) or (c ##1 d))
      or_pass++;
    else
      or_fail++;
  a_and: assert property (@(posedge clk) (c ##1 d) and (a ##1 1'b1))
      and_pass++;
    else
      and_fail++;
  a_implies: assert property (@(posedge clk) (a ##1 b) implies (c ##1 d))
      implies_pass++;
    else
      implies_fail++;
  a_iff: assert property (@(posedge clk) (a ##1 b) iff (c ##1 d))
      iff_pass++;
    else
      iff_fail++;
  a_consequent: assert property (@(posedge clk) a |-> not (c ##1 d))
      consequent_pass++;
    else
      consequent_fail++;
  a_operands: assert property (@(posedge clk) (a |=> b) and (c |=> d))
      operands_pass++;
    else
      operands_fail++;
  a_negated: assert property (@(posedge clk) not (a |=> b))
      negated_pass++;
    else
      negated_fail++;
  c_not: cover property (@(posedge clk) not (a ##1 b)) covered++;

  // An expect reads its sequences strongly (Annex F.3.4.3.1). Reached at 2,
  // the first starts at the tick at 5 and holds at 15, where `c ##1 d`
  // matches. Reached at 22, the second starts at 25 and fails at 35, where
  // `a ##1 b` matches.
  time held_at = -1;
  int held_runs = 0;
  initial begin
    #2;
    expect (@(posedge clk) (a ##1 b) or (c ##1 d)) held_runs++;
      else held_runs = -100;
    held_at = $time;
  end
  time failed_at = -1;
  int failed_runs = 0;
  initial begin
    #22;
    expect (@(posedge clk) not (a ##1 b)) failed_runs = -100;
      else failed_runs++;
    failed_at = $time;
  end

  function automatic void check(string name, int passes, int fails,
                                int want_passes, int want_fails);
    if (passes !== want_passes || fails !== want_fails)
      $fatal(1, "%s held %0d and failed %0d times, expected %0d and %0d",
             name, passes, fails, want_passes, want_fails);
  endfunction

  final begin
    // The match at k = 2 fails that attempt; the attempt at k = 5 is still
    // able to match when the run ends, which fails it.
    check("not", not_pass, not_fail, 4, 2);
    // Both operands fail at k = 1 and k = 4; the attempt at k = 5 ends with
    // its weak left operand able to match, which holds.
    check("or", or_pass, or_fail, 4, 2);
    check("and", and_pass, and_fail, 2, 4);
    check("implies", implies_pass, implies_fail, 4, 2);
    check("iff", iff_pass, iff_fail, 2, 4);
    // `c ##1 d` matches from k = 0 and k = 3, where `a` holds.
    check("not as a consequent", consequent_pass, consequent_fail, 4, 2);
    // At k = 5 the run ends before the tick after `a`, the last tick of the
    // left implication's antecedent, so it holds with no match.
    check("implications as operands", operands_pass, operands_fail, 3, 3);
    check("a negated implication", negated_pass, negated_fail, 2, 4);
    if (covered !== 5)
      $fatal(1, "the negated sequence was covered %0d times, expected 5",
             covered);
    if (held_runs !== 1 || held_at !== 15)
      $fatal(1, "the expect on an or ran %0d and went on at %0t", held_runs,
             held_at);
    if (failed_runs !== 1 || failed_at !== 35)
      $fatal(1, "the expect on a not ran %0d and went on at %0t",
             failed_runs, failed_at);
    $display("All checks passed");
  end
endmodule

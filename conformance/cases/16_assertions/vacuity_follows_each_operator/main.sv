// Whether a success is vacuous is defined operator by operator (LRM 16.14.8):
// `p1 implies p2` is nonvacuous only where `p1` is true and both operands are
// nonvacuous, a disjunction and `iff` where either operand is, and an
// implication where its antecedent matched. `$assertvacuousoff` keeps a
// vacuous success from running the pass statement (LRM 20.11), so each count
// below is of the nonvacuous successes.
//
// The last two assertions succeed at the tick `d` holds, while their left
// operand, a sequence one tick long, has not yet matched. A success is
// reported at the tick it is decided (LRM 16.14.3), so `p1` is read on the
// trace up to that tick, as a property on a finite trace is read (Annex
// F.5.3.1): a weak sequence still able to match holds there, and a strong one
// does not, so only the weak one's success is nonvacuous. What the left
// operand later comes to does not reach a success already reported.
//
// Ticks land at 10k + 5, and every value is written at a multiple of 10,
// between ticks.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  logic c = 0;
  logic d = 0;
  logic e = 0;
  int implied = 0;
  int either = 0;
  int both_ways = 0;
  int weak_left = 0;
  int strong_left = 0;
  int fails = 0;

  always #5 clk = ~clk;

  assert property (@(posedge clk) c implies d)
    implied = implied + 1;
  else
    fails = fails + 1;
  assert property (@(posedge clk) (a |-> b) or (c |-> d))
    either = either + 1;
  else
    fails = fails + 1;
  assert property (@(posedge clk) (a |-> b) iff (c |-> d))
    both_ways = both_ways + 1;
  else
    fails = fails + 1;
  assert property (@(posedge clk) weak(c ##1 e) implies d)
    weak_left = weak_left + 1;
  else
    fails = fails + 1;
  assert property (@(posedge clk) strong(c ##1 e) implies d)
    strong_left = strong_left + 1;
  else
    fails = fails + 1;

  task automatic Expect(
      input int want_implied, input int want_either, input int want_both_ways,
      input int want_weak_left, input string step);
    if (implied !== want_implied || either !== want_either ||
        both_ways !== want_both_ways || weak_left !== want_weak_left ||
        strong_left !== 0 || fails !== 0) begin
      $fatal(1, "%s: %0d %0d %0d %0d %0d passes and %0d fails", step, implied,
             either, both_ways, weak_left, strong_left, fails);
    end
  endtask

  initial begin
    $assertvacuousoff;

    // At 5 every left operand and antecedent is false.
    #10;
    Expect(0, 0, 0, 0, "every success vacuous");

    // At 15 `c` and `d` hold and `a` does not.
    c = 1;
    d = 1;
    #10;
    Expect(1, 1, 1, 1, "the right operand nonvacuous");

    // At 25 nothing holds again, and `e` never does.
    c = 0;
    d = 0;
    #10;
    Expect(1, 1, 1, 1, "vacuous again");

    // At 35 `a` and `b` hold and `c` does not.
    a = 1;
    b = 1;
    #10;
    Expect(1, 2, 2, 1, "the left operand nonvacuous");

    a = 0;
    b = 0;
    #10;
    Expect(1, 2, 2, 1, "vacuous at the end");
    $display("All checks passed");
    $finish;
  end
endmodule

// An implication and a logical equivalence each take the rest of their run as
// the second operand (LRM Table 11-2, 11.4.7), to any length. Three hundred
// answer as two do: the last operand's value comes back through every one of
// them, an implication whose first operand is false answers 1 without
// evaluating what follows it (LRM 11.3.5) while one whose first operand is
// unknown goes on to it, and an equivalence evaluates both.
//
// The chains of one repeated operand are spelled by a macro.

`define TEN(x) x x x x x x x x x x
`define HUNDRED(x) `TEN(`TEN(x))
`define THREE_HUNDRED(x) `HUNDRED(x) `HUNDRED(x) `HUNDRED(x)

module Top;
  bit z, t;
  logic lt, lx;
  bit implied;
  logic implied4;
  bit same_ones, same_zeros;
  logic same_unknown;
  bit skipped;
  logic reached;
  int calls;

  function automatic bit called();
    calls++;
    return 1;
  endfunction

  initial begin
    z = 0;
    t = 1;
    lt = 1'b1;
    lx = 1'bx;
    calls = 0;

    implied = `THREE_HUNDRED(t ->) z;
    implied4 = `THREE_HUNDRED(lt ->) lx;
    if (implied !== 1'b0) $fatal(1, "the implication was %b, expected 0", implied);
    if (implied4 !== 1'bx) $fatal(1, "the four-state implication was %b", implied4);

    // An odd number of zeros: the innermost pair is equivalent, and each zero
    // before it turns the answer over once more.
    same_ones = `THREE_HUNDRED(t <->) t;
    same_zeros = `THREE_HUNDRED(z <->) z;
    same_unknown = lx <-> `THREE_HUNDRED(lt <->) lt;
    if (same_ones !== 1'b1) $fatal(1, "the equivalence of ones was %b", same_ones);
    if (same_zeros !== 1'b0) $fatal(1, "the equivalence of 301 zeros was %b", same_zeros);
    if (same_unknown !== 1'bx) $fatal(1, "an equivalence with x was %b", same_unknown);

    skipped = `THREE_HUNDRED(t ->) z -> called();
    if (skipped !== 1'b1) $fatal(1, "a false first operand answered %b", skipped);
    if (calls !== 0) $fatal(1, "an operand after a false one ran %0d times", calls);

    reached = `THREE_HUNDRED(lt ->) lx -> called();
    if (reached !== 1'b1) $fatal(1, "an unknown first operand answered %b", reached);
    if (calls !== 1) $fatal(1, "an operand after an unknown one ran %0d times", calls);

    $display("All checks passed");
  end
endmodule

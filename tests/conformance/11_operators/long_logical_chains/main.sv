// A logical operator takes any number of operands (LRM 11.2), and one of three
// hundred stops where its answer is settled, as one of two does (LRM 11.3.5,
// 11.4.7): an operand after that is not evaluated, and an unknown operand
// leaves the answer to the ones after it.
//
// The chains of one repeated operand are spelled by a macro.

`define TEN(x) x x x x x x x x x x
`define HUNDRED(x) `TEN(`TEN(x))
`define THREE_HUNDRED(x) `HUNDRED(x) `HUNDRED(x) `HUNDRED(x)

module Top;
  bit z, t;
  logic lz, lt, lx;
  bit every, one_false;
  logic any4, open_or, settled_or;
  logic every4, open_and, settled_and;
  bit skipped_or, skipped_and;
  logic skipped_or4, skipped_and4;
  logic grouped_or, grouped_and;
  int calls;

  function automatic bit called();
    calls++;
    return 1;
  endfunction

  initial begin
    z = 0;
    t = 1;
    lz = 1'b0;
    lt = 1'b1;
    lx = 1'bx;
    calls = 0;

    every = `THREE_HUNDRED(t &&) t;
    one_false = `THREE_HUNDRED(t &&) z;
    if (every !== 1'b1) $fatal(1, "the logical and was %b, expected 1", every);
    if (one_false !== 1'b0) $fatal(1, "the and ending in 0 was %b", one_false);

    any4 = `THREE_HUNDRED(lz ||) lt;
    open_or = lx || `THREE_HUNDRED(lz ||) lz;
    settled_or = lx || `THREE_HUNDRED(lz ||) lt;
    if (any4 !== 1'b1) $fatal(1, "the four-state or was %b, expected 1", any4);
    if (open_or !== 1'bx) $fatal(1, "x or zeros was %b, expected x", open_or);
    if (settled_or !== 1'b1) $fatal(1, "x or a one was %b, expected 1", settled_or);

    every4 = `THREE_HUNDRED(lt &&) lt;
    open_and = lx && `THREE_HUNDRED(lt &&) lt;
    settled_and = lx && `THREE_HUNDRED(lt &&) lz;
    if (every4 !== 1'b1) $fatal(1, "the four-state and was %b, expected 1", every4);
    if (open_and !== 1'bx) $fatal(1, "x and ones was %b, expected x", open_and);
    if (settled_and !== 1'b0) $fatal(1, "x and a zero was %b, expected 0", settled_and);

    skipped_or = `THREE_HUNDRED(z ||) t || called();
    skipped_and = `THREE_HUNDRED(t &&) z && called();
    skipped_or4 = lx || `THREE_HUNDRED(lz ||) lt || called();
    skipped_and4 = lx && `THREE_HUNDRED(lt &&) lz && called();
    if (skipped_or !== 1'b1 || skipped_and !== 1'b0) $fatal(1, "a settled chain answered wrongly");
    if (skipped_or4 !== 1'b1 || skipped_and4 !== 1'b0) $fatal(1, "a settled four-state chain answered wrongly");

    // Parentheses that group a run from its last operand change neither.
    grouped_or = lz || (lx || (lt || called()));
    grouped_and = lt && (lx && (lz && called()));
    if (grouped_or !== 1'b1 || grouped_and !== 1'b0) $fatal(1, "a chain grouped from the right answered wrongly");
    if (calls !== 0) $fatal(1, "an operand after the answer was settled ran %0d times", calls);

    $display("All checks passed");
  end
endmodule

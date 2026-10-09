// A conditional operator's third operand may itself be a conditional, to any
// length (LRM 11.4.11). Three hundred arms select exactly as three do: an arm
// whose predicate is false passes the selection on, the arm whose predicate is
// true ends it, and nothing after that arm is evaluated. A predicate that is
// unknown selects neither of its operands, so its arm's value is combined bit
// by bit with what the rest of the chain answers.
//
// The arms are spelled by macros, so each chain is written once and expands to
// three hundred arms.

`define KNOWN(n) two == n ? n :
`define KNOWN10(t) `KNOWN(t``0) `KNOWN(t``1) `KNOWN(t``2) `KNOWN(t``3) `KNOWN(t``4) `KNOWN(t``5) `KNOWN(t``6) `KNOWN(t``7) `KNOWN(t``8) `KNOWN(t``9)
`define KNOWN100(h) `KNOWN10(h``0) `KNOWN10(h``1) `KNOWN10(h``2) `KNOWN10(h``3) `KNOWN10(h``4) `KNOWN10(h``5) `KNOWN10(h``6) `KNOWN10(h``7) `KNOWN10(h``8) `KNOWN10(h``9)

`define FOUR(n) four == n ? n :
`define FOUR10(t) `FOUR(t``0) `FOUR(t``1) `FOUR(t``2) `FOUR(t``3) `FOUR(t``4) `FOUR(t``5) `FOUR(t``6) `FOUR(t``7) `FOUR(t``8) `FOUR(t``9)
`define FOUR100(h) `FOUR10(h``0) `FOUR10(h``1) `FOUR10(h``2) `FOUR10(h``3) `FOUR10(h``4) `FOUR10(h``5) `FOUR10(h``6) `FOUR10(h``7) `FOUR10(h``8) `FOUR10(h``9)

module Top;
  int two;
  logic [31:0] four;
  logic unknown;
  int picked;
  logic [31:0] picked4, combined, ended;
  int calls;

  function automatic int called();
    calls++;
    return 0;
  endfunction

  initial begin
    two = 1299;
    four = 1299;
    unknown = 1'bx;
    calls = 0;

    picked = `KNOWN100(10) `KNOWN100(11) `KNOWN100(12) -1;
    if (picked !== 1299) $fatal(1, "the two-state chain picked %0d", picked);

    picked4 = `FOUR100(10) `FOUR100(11) `FOUR100(12) 32'hffff_ffff;
    if (picked4 !== 1299) $fatal(1, "the four-state chain picked %0d", picked4);

    // The first predicate is unknown, so its value is combined with 32'h513,
    // which the rest of the chain selects: every bit the two agree on is kept.
    combined = unknown ? 32'h0000_0513 : `FOUR100(10) `FOUR100(11) `FOUR100(12) 32'h0;
    if (combined !== 32'h0000_0513) $fatal(1, "equal arms combined to %h", combined);
    combined = unknown ? 32'h0000_051c : `FOUR100(10) `FOUR100(11) `FOUR100(12) 32'h0;
    if (combined !== 32'h0000_051x) $fatal(1, "differing arms combined to %h", combined);

    // The selected arm ends the chain: neither a later predicate nor a later
    // value is evaluated.
    four = 1000;
    ended = `FOUR100(10) `FOUR100(11) `FOUR100(12)
        called() == 0 ? called() : called();
    if (ended !== 1000) $fatal(1, "the first arm was not taken, got %0d", ended);
    if (calls !== 0) $fatal(1, "an arm after the selected one ran %0d times", calls);

    $display("All checks passed");
  end
endmodule

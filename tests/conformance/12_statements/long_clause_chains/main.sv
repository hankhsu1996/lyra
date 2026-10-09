// An if-else-if construct takes any number of arms, each predicate a series of
// clauses joined by `&&&`, a clause possibly matching a pattern and a later one
// reading what it bound (LRM 12.4, 12.6.2). Three hundred arms select exactly
// as three do: an arm whose predicate fails at any of its clauses passes the
// test on to the next, and the last arm is reached when nothing before it held.
// A single predicate takes any number of clauses the same way.
//
// The arms are spelled by macros, so each chain is written once and expands to
// three hundred arms after a first that never holds.

`define PLAIN(n) else if (sel == n &&& on) plain = n;
`define PLAIN10(t) `PLAIN(t``0) `PLAIN(t``1) `PLAIN(t``2) `PLAIN(t``3) `PLAIN(t``4) `PLAIN(t``5) `PLAIN(t``6) `PLAIN(t``7) `PLAIN(t``8) `PLAIN(t``9)
`define PLAIN100(h) `PLAIN10(h``0) `PLAIN10(h``1) `PLAIN10(h``2) `PLAIN10(h``3) `PLAIN10(h``4) `PLAIN10(h``5) `PLAIN10(h``6) `PLAIN10(h``7) `PLAIN10(h``8) `PLAIN10(h``9)

`define FILTER(n) else if (v matches tagged Hit .x &&& x == n) filtered = n;
`define FILTER10(t) `FILTER(t``0) `FILTER(t``1) `FILTER(t``2) `FILTER(t``3) `FILTER(t``4) `FILTER(t``5) `FILTER(t``6) `FILTER(t``7) `FILTER(t``8) `FILTER(t``9)
`define FILTER100(h) `FILTER10(h``0) `FILTER10(h``1) `FILTER10(h``2) `FILTER10(h``3) `FILTER10(h``4) `FILTER10(h``5) `FILTER10(h``6) `FILTER10(h``7) `FILTER10(h``8) `FILTER10(h``9)

`define CLAUSES10(x) x x x x x x x x x x
`define CLAUSES100(x) `CLAUSES10(`CLAUSES10(x))
`define CLAUSES300(x) `CLAUSES100(x) `CLAUSES100(x) `CLAUSES100(x)

module Top;
  typedef union tagged {
    int Other;
    int Hit;
  } value_t;

  initial begin
    int sel;
    bit on;
    value_t v;
    int plain;
    int filtered;
    logic known, unknown;
    int wide;
    sel = 299;
    on = 1;
    known = 1'b1;
    unknown = 1'bx;
    v = tagged Hit 299;

    // Each arm is two clauses, so it can fail at either one.
    if (sel < 0 &&& on) plain = -2;
    `PLAIN10() `PLAIN10(1) `PLAIN10(2) `PLAIN10(3) `PLAIN10(4)
    `PLAIN10(5) `PLAIN10(6) `PLAIN10(7) `PLAIN10(8) `PLAIN10(9)
    `PLAIN100(1) `PLAIN100(2)
    else plain = -1;
    if (plain !== 299) $fatal(1, "the &&& chain took arm %0d, expected 299", plain);

    // Each arm's pattern matches and its filter reads the binding, so every arm
    // but the last fails at its second clause.
    if (v matches tagged Other .x) filtered = -2;
    `FILTER10() `FILTER10(1) `FILTER10(2) `FILTER10(3) `FILTER10(4)
    `FILTER10(5) `FILTER10(6) `FILTER10(7) `FILTER10(8) `FILTER10(9)
    `FILTER100(1) `FILTER100(2)
    else filtered = -1;
    if (filtered !== 299) $fatal(1, "the filtered chain took %0d, expected 299", filtered);

    // One predicate of three hundred clauses holds when every clause does, and
    // a clause that is unknown ends it as not holding.
    wide = 0;
    if (`CLAUSES300(on &&&) on) wide = 1;
    if (wide !== 1) $fatal(1, "a predicate of three hundred true clauses did not hold");
    if (`CLAUSES300(known &&&) known) wide = 2;
    if (wide !== 2) $fatal(1, "a four-state predicate of true clauses did not hold");
    if (unknown &&& `CLAUSES300(known &&&) known) wide = 3;
    if (wide !== 2) $fatal(1, "a predicate with an unknown clause held");

    $display("All checks passed");
  end
endmodule

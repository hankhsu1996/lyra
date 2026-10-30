// An if-else-if construct may test a pattern in every arm, and a conditional
// expression's else arm may itself be a conditional testing a pattern (LRM
// 12.6.2, 12.6.3). Three hundred arms of each select exactly as three do: an
// arm whose pattern does not match passes the test on to the next, and the
// last arm is reached when nothing before it matched.
//
// The arms are spelled by macros, so each chain is written once and expands to
// three hundred arms after a first that never matches.

`define TAG(n) else if (v matches tagged Other .x) matched = n;
`define TAG10(t) `TAG(t``0) `TAG(t``1) `TAG(t``2) `TAG(t``3) `TAG(t``4) `TAG(t``5) `TAG(t``6) `TAG(t``7) `TAG(t``8) `TAG(t``9)
`define TAG100(h) `TAG10(h``0) `TAG10(h``1) `TAG10(h``2) `TAG10(h``3) `TAG10(h``4) `TAG10(h``5) `TAG10(h``6) `TAG10(h``7) `TAG10(h``8) `TAG10(h``9)

`define COND(n) v matches tagged Other .x ? n :
`define COND10(t) `COND(t``0) `COND(t``1) `COND(t``2) `COND(t``3) `COND(t``4) `COND(t``5) `COND(t``6) `COND(t``7) `COND(t``8) `COND(t``9)
`define COND100(h) `COND10(h``0) `COND10(h``1) `COND10(h``2) `COND10(h``3) `COND10(h``4) `COND10(h``5) `COND10(h``6) `COND10(h``7) `COND10(h``8) `COND10(h``9)

module Top;
  typedef union tagged {
    int Other;
    int Hit;
  } value_t;

  initial begin
    value_t v;
    int matched;
    int chosen;
    v = tagged Hit 299;

    // Each arm matches a member the value does not hold, until the last.
    if (v matches tagged Other .x) matched = -2;
    `TAG10() `TAG10(1) `TAG10(2) `TAG10(3) `TAG10(4)
    `TAG10(5) `TAG10(6) `TAG10(7) `TAG10(8) `TAG10(9)
    `TAG100(1) `TAG100(2)
    else if (v matches tagged Hit .x) matched = x;
    else matched = -1;
    if (matched !== 299) $fatal(1, "the pattern chain took %0d, expected 299", matched);

    chosen = v matches tagged Hit .x &&& x < 0 ? -2 :
      `COND10() `COND10(1) `COND10(2) `COND10(3) `COND10(4)
      `COND10(5) `COND10(6) `COND10(7) `COND10(8) `COND10(9)
      `COND100(1) `COND100(2)
      v matches tagged Hit .x ? x : -1;
    if (chosen !== 299) $fatal(1, "the conditional chain chose %0d, expected 299", chosen);

    $display("All checks passed");
  end
endmodule

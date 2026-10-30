// Operators of one precedence group from the left (LRM Table 11-2), so in a run
// of them each is applied to the answer of the ones before it. A run of three
// hundred that alternates two operators of one level, or repeats an inequality
// whose answer is the next one's first operand, answers as it would written
// out link by link.
//
// The chains of one repeated operand are spelled by a macro.

`define TEN(x) x x x x x x x x x x
`define HUNDRED(x) `TEN(`TEN(x))
`define THREE_HUNDRED(x) `HUNDRED(x) `HUNDRED(x) `HUNDRED(x)

module Top;
  int v;
  bit z, t;
  logic lz, lt, lx;
  bit differs;
  logic differs4, differs_unknown;
  int mixed_xor;
  logic mixed_equal;

  initial begin
    v = 1;
    z = 0;
    t = 1;
    lz = 1'b0;
    lt = 1'b1;
    lx = 1'bx;

    // Zeros compare equal until the last operand, a one.
    differs = `THREE_HUNDRED(z !==) t;
    differs4 = `THREE_HUNDRED(lz !=?) lt;
    differs_unknown = lx !=? `THREE_HUNDRED(lz !=?) lt;
    if (differs !== 1'b1) $fatal(1, "the case inequality chain was %b", differs);
    if (differs4 !== 1'b1) $fatal(1, "the wildcard inequality chain was %b", differs4);
    if (differs_unknown !== 1'bx) $fatal(1, "a wildcard inequality of x was %b", differs_unknown);

    // Four of these bring 1 back to 1, and there are six hundred.
    mixed_xor = `THREE_HUNDRED(v ^ v ~^) v;
    if (mixed_xor !== 1) $fatal(1, "the xor and xnor chain was %h", mixed_xor);

    // Each `==` of two zeros answers 1 and each `==?` of that with a zero
    // answers 0, until the last, against a one.
    mixed_equal = `THREE_HUNDRED(lz == lz ==?) lt;
    if (mixed_equal !== 1'b1) $fatal(1, "the mixed equality chain was %b", mixed_equal);

    $display("All checks passed");
  end
endmodule

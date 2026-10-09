// An expression takes any number of operands (LRM 11.2), and a run of one
// binary operator groups from the left (LRM Table 11-2), so its first operand
// is as deep in the expression as the run is long. A sum and a logical or of
// three thousand operands each evaluate like their short forms.
//
// The chains of one repeated operand are spelled by a macro.

`define TEN(x) x x x x x x x x x x
`define THOUSAND(x) `TEN(`TEN(`TEN(x)))
`define THREE_THOUSAND(x) `THOUSAND(x) `THOUSAND(x) `THOUSAND(x)

module Top;
  int v;
  int sum;
  bit z, t;
  bit any_true, none_true;
  logic lz, lt;
  logic any_true4;

  initial begin
    v = 1;
    z = 0;
    t = 1;
    lz = 1'b0;
    lt = 1'b1;

    sum = `THREE_THOUSAND(v +) v;
    if (sum !== 3001) $fatal(1, "the sum was %0d, expected 3001", sum);

    any_true = `THREE_THOUSAND(z ||) t;
    none_true = `THREE_THOUSAND(z ||) z;
    if (any_true !== 1'b1) $fatal(1, "the logical or ending in 1 was %b", any_true);
    if (none_true !== 1'b0) $fatal(1, "the logical or of zeros was %b", none_true);

    any_true4 = `THREE_THOUSAND(lz ||) lt;
    if (any_true4 !== 1'b1) $fatal(1, "the four-state logical or was %b", any_true4);

    $display("All checks passed");
  end
endmodule

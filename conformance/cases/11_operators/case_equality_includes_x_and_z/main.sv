// The case equality operator === compares its operands bit for bit, and bits
// holding x or z are included in that comparison and have to match for the
// operands to be equal; the result is always a known 1'b1 or 1'b0 and never x,
// whatever the operands hold. The case inequality operator !== is its negation
// (LRM 11.4.5).
module Top;
  int a;
  int b;
  logic [3:0] all_x;
  logic [3:0] all_z;
  logic [3:0] mixed;

  logic same_eq;
  logic same_neq;
  logic differ_eq;
  logic differ_neq;
  logic x_against_x;
  logic z_against_z;
  logic x_against_z;
  logic mixed_eq_self;
  logic mixed_neq_self;
  logic x_against_one;
  logic z_against_one;

  initial begin
    a = 5;
    b = 5;
    same_eq = (a === b);
    same_neq = (a !== b);

    b = 10;
    differ_eq = (a === b);
    differ_neq = (a !== b);

    all_x = 4'bxxxx;
    all_z = 4'bzzzz;
    mixed = 4'b10x1;

    x_against_x = (all_x === all_x);
    z_against_z = (all_z === all_z);
    x_against_z = (all_x === all_z);
    mixed_eq_self = (mixed === mixed);
    mixed_neq_self = (mixed !== mixed);
    x_against_one = (mixed === 4'b1011);
    z_against_one = (4'b10z1 === 4'b1011);
  end

  final begin
    if (same_eq !== 1'b1) $fatal(1, "same_eq was %b, expected 1", same_eq);
    if (same_neq !== 1'b0) $fatal(1, "same_neq was %b, expected 0", same_neq);
    if (differ_eq !== 1'b0)
      $fatal(1, "differ_eq was %b, expected 0", differ_eq);
    if (differ_neq !== 1'b1)
      $fatal(1, "differ_neq was %b, expected 1", differ_neq);
    if (x_against_x !== 1'b1)
      $fatal(1, "x_against_x was %b, expected 1", x_against_x);
    if (z_against_z !== 1'b1)
      $fatal(1, "z_against_z was %b, expected 1", z_against_z);
    if (x_against_z !== 1'b0)
      $fatal(1, "x_against_z was %b, expected 0", x_against_z);
    if (mixed_eq_self !== 1'b1)
      $fatal(1, "mixed_eq_self was %b, expected 1", mixed_eq_self);
    if (mixed_neq_self !== 1'b0)
      $fatal(1, "mixed_neq_self was %b, expected 0", mixed_neq_self);
    if (x_against_one !== 1'b0)
      $fatal(1, "x_against_one was %b, expected 0", x_against_one);
    if (z_against_one !== 1'b0)
      $fatal(1, "z_against_one was %b, expected 0", z_against_one);
    $display("All checks passed");
  end
endmodule

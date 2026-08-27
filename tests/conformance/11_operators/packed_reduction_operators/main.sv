// A unary reduction operator applies a bitwise logic table across every bit
// of one operand and yields a single bit: & | and ^ combine the bits in turn
// from the first, and ~& ~| and ~^ invert the result the corresponding
// operator would give. A bit that settles the outcome on its own -- a 0 under
// & or a 1 under | -- keeps the result known even when other bits are x or z;
// any other unknown bit makes the result x (LRM 11.4.9,
// Tables 11-16 to 11-19).
module Top;
  bit and_all_ones;
  bit and_one_zero;
  bit or_all_zeros;
  bit or_one_bit;
  bit xor_odd_ones;
  bit xor_even_ones;
  bit nand_all_ones;
  bit nor_all_zeros;
  bit xnor_odd_ones;
  bit and_odd_width_all_ones;
  bit and_odd_width_top_zero;
  bit or_odd_width_top_one;
  logic and_with_unknown;
  logic and_zero_settles;
  logic or_with_high_impedance;
  logic or_one_settles;
  logic xor_with_unknown;
  logic xnor_with_high_impedance;
  logic and_reg_with_high_impedance;

  initial begin
    bit [3:0] p;
    bit [4:0] odd_width;
    logic [3:0] q;
    reg [3:0] r;

    and_one_zero = 1'b1;
    or_all_zeros = 1'b1;
    xor_even_ones = 1'b1;
    nand_all_ones = 1'b1;
    xnor_odd_ones = 1'b1;
    and_odd_width_top_zero = 1'b1;
    and_with_unknown = 1'b0;
    or_with_high_impedance = 1'b0;
    xor_with_unknown = 1'b0;
    xnor_with_high_impedance = 1'b0;
    and_reg_with_high_impedance = 1'b0;

    p = 4'b1111;
    and_all_ones = &p;
    nand_all_ones = ~&p;
    p = 4'b1101;
    and_one_zero = &p;
    p = 4'b0000;
    or_all_zeros = |p;
    nor_all_zeros = ~|p;
    p = 4'b0100;
    or_one_bit = |p;
    p = 4'b1011;
    xor_odd_ones = ^p;
    xnor_odd_ones = ~^p;
    p = 4'b1010;
    xor_even_ones = ^p;

    // A width that is not a whole number of bytes, so bits above the declared
    // range cannot pass for part of the operand.
    odd_width = 5'b11111;
    and_odd_width_all_ones = &odd_width;
    odd_width = 5'b01111;
    and_odd_width_top_zero = &odd_width;
    odd_width = 5'b10000;
    or_odd_width_top_one = |odd_width;

    q = 4'b11x1;
    and_with_unknown = &q;
    q = 4'b10x1;
    and_zero_settles = &q;
    q = 4'b00z0;
    or_with_high_impedance = |q;
    q = 4'b01z0;
    or_one_settles = |q;
    q = 4'b10x1;
    xor_with_unknown = ^q;
    q = 4'b10z1;
    xnor_with_high_impedance = ~^q;

    r = 4'b11z1;
    and_reg_with_high_impedance = &r;
  end

  final begin
    if (and_all_ones !== 1'b1)
      $fatal(1, "and_all_ones was %b, expected 1", and_all_ones);
    if (and_one_zero !== 1'b0)
      $fatal(1, "and_one_zero was %b, expected 0", and_one_zero);
    if (or_all_zeros !== 1'b0)
      $fatal(1, "or_all_zeros was %b, expected 0", or_all_zeros);
    if (or_one_bit !== 1'b1)
      $fatal(1, "or_one_bit was %b, expected 1", or_one_bit);
    if (xor_odd_ones !== 1'b1)
      $fatal(1, "xor_odd_ones was %b, expected 1", xor_odd_ones);
    if (xor_even_ones !== 1'b0)
      $fatal(1, "xor_even_ones was %b, expected 0", xor_even_ones);
    if (nand_all_ones !== 1'b0)
      $fatal(1, "nand_all_ones was %b, expected 0", nand_all_ones);
    if (nor_all_zeros !== 1'b1)
      $fatal(1, "nor_all_zeros was %b, expected 1", nor_all_zeros);
    if (xnor_odd_ones !== 1'b0)
      $fatal(1, "xnor_odd_ones was %b, expected 0", xnor_odd_ones);
    if (and_odd_width_all_ones !== 1'b1)
      $fatal(1, "and_odd_width_all_ones was %b, expected 1",
             and_odd_width_all_ones);
    if (and_odd_width_top_zero !== 1'b0)
      $fatal(1, "and_odd_width_top_zero was %b, expected 0",
             and_odd_width_top_zero);
    if (or_odd_width_top_one !== 1'b1)
      $fatal(1, "or_odd_width_top_one was %b, expected 1",
             or_odd_width_top_one);
    if (and_with_unknown !== 1'bx)
      $fatal(1, "and_with_unknown was %b, expected x", and_with_unknown);
    if (and_zero_settles !== 1'b0)
      $fatal(1, "and_zero_settles was %b, expected 0", and_zero_settles);
    if (or_with_high_impedance !== 1'bx)
      $fatal(1, "or_with_high_impedance was %b, expected x",
             or_with_high_impedance);
    if (or_one_settles !== 1'b1)
      $fatal(1, "or_one_settles was %b, expected 1", or_one_settles);
    if (xor_with_unknown !== 1'bx)
      $fatal(1, "xor_with_unknown was %b, expected x", xor_with_unknown);
    if (xnor_with_high_impedance !== 1'bx)
      $fatal(1, "xnor_with_high_impedance was %b, expected x",
             xnor_with_high_impedance);
    if (and_reg_with_high_impedance !== 1'bx)
      $fatal(1, "and_reg_with_high_impedance was %b, expected x",
             and_reg_with_high_impedance);
    $display("All checks passed");
  end
endmodule

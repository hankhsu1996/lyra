// A packed tagged union is a vector as wide as its tag plus the widest of its
// members, where the tag is the smallest number of bits that can name every
// member. The tag bits are the most significant of that vector and the
// member's own bits the least significant, and the bits left between the two
// by a member narrower than the widest are undefined. Distinct members carry
// distinct tags, and for a union of a void member followed by an int member
// the standard gives the two tags outright: 0 for the void member and 1 for
// the other. The scheme applies again to a nested tagged union. A packed
// union holding both a 2-state and a 4-state member is 4-state throughout
// (LRM 7.3.2, Figure 7-2, 7.3.1).
module Top;
  typedef union tagged packed {
    void Invalid;
    int  Valid;
  } vint_t;

  // Five members need three bits to name and the widest is twelve.
  typedef union tagged packed {
    bit [4:0]  A;
    bit [9:0]  B;
    bit [1:0]  C;
    bit [11:0] D;
    bit [2:0]  E;
  } five_t;

  typedef struct packed {
    bit [3:0] high;
    bit [3:0] low;
  } pair_t;

  typedef union tagged packed {
    bit [7:0] Byte;
    bit [3:0] Nib;
  } inner_t;

  // The wider member is itself a packed tagged union, so it contributes its
  // own tag to the width.
  typedef union tagged packed {
    pair_t  Pair;
    inner_t Inner;
  } outer_t;

  typedef union tagged packed {
    logic [7:0] Unknown;
    bit   [3:0] Known;
  } mixed_t;

  int vint_width;
  int five_width;
  int outer_width;

  bit [32:0] raw_valid;
  bit [32:0] raw_invalid = 33'h1FFFFFFFF;
  bit [14:0] raw_b;
  bit [14:0] raw_d;
  bit [9:0] raw_nib;
  bit [9:0] raw_byte;
  logic [8:0] raw_mixed;

  initial begin
    vint_t valid;
    vint_t invalid;
    five_t five;
    outer_t outer;
    mixed_t mixed;

    vint_width = $bits(vint_t);
    five_width = $bits(five_t);
    outer_width = $bits(outer_t);

    valid = tagged Valid 42;
    raw_valid = valid;
    invalid = tagged Invalid;
    raw_invalid = invalid;

    five = tagged B 10'h2AA;
    raw_b = five;
    five = tagged D 12'hABC;
    raw_d = five;

    outer = tagged Inner (tagged Nib 4'hC);
    raw_nib = outer;
    outer = tagged Inner (tagged Byte 8'hA5);
    raw_byte = outer;

    mixed = tagged Unknown 8'b1010xxxx;
    raw_mixed = mixed;
  end

  final begin
    if (vint_width !== 33)
      $fatal(1, "a one bit tag over a 32 bit member gave %0d bits, expected 33",
             vint_width);
    if (five_width !== 15)
      $fatal(1,
             "a three bit tag over a 12 bit member gave %0d bits, expected 15",
             five_width);
    if (outer_width !== 10)
      $fatal(1, "a tag over a nested tagged union gave %0d bits, expected 10",
             outer_width);

    // The member's bits are the least significant ones.
    if (raw_valid[31:0] !== 32'd42)
      $fatal(1, "the member bits read %0h, expected 2a", raw_valid[31:0]);
    if (raw_b[9:0] !== 10'h2AA)
      $fatal(1, "a narrow member's bits read %0h, expected 2aa", raw_b[9:0]);
    if (raw_d[11:0] !== 12'hABC)
      $fatal(1, "the widest member's bits read %0h, expected abc",
             raw_d[11:0]);

    // The tag bits are the most significant ones. Figure 7-2 gives the tags
    // for a void member and an int member; elsewhere what the standard fixes
    // is that members differ, so the vector says which one the value is.
    if (raw_invalid[32] !== 1'b0)
      $fatal(1, "the void member's tag bit was %b, expected 0",
             raw_invalid[32]);
    if (raw_valid[32] !== 1'b1)
      $fatal(1, "the int member's tag bit was %b, expected 1", raw_valid[32]);
    if (raw_b[14:12] === raw_d[14:12])
      $fatal(1, "two members shared the tag bits %b", raw_b[14:12]);

    // Applied again one level down: the nested union's own tag sits at the top
    // of the bits the outer union gives it, and its member's bits at the
    // bottom.
    if (raw_nib[3:0] !== 4'hC)
      $fatal(1, "a nested member's bits read %0h, expected c", raw_nib[3:0]);
    if (raw_byte[7:0] !== 8'hA5)
      $fatal(1, "the wider nested member's bits read %0h, expected a5",
             raw_byte[7:0]);
    if (raw_nib[8] === raw_byte[8])
      $fatal(1, "two nested members shared the tag bit %b", raw_nib[8]);

    // A 4-state member makes the whole union 4-state, so unknown bits survive
    // being read out of it.
    if (raw_mixed[7:4] !== 4'b1010)
      $fatal(1, "the known half read %b, expected 1010", raw_mixed[7:4]);
    if (raw_mixed[3:0] !== 4'bxxxx)
      $fatal(1, "the unknown half read %b, expected xxxx", raw_mixed[3:0]);
    $display("All checks passed");
  end
endmodule

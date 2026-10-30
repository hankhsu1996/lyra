// A packed structure subdivides a vector into members packed together without
// gaps, the first member declared taking the most significant bits and each
// later member the next most significant. Assigning the structure as a whole
// and assigning its members one at a time therefore reach the same bits: a
// whole-structure value can be read back member by member, members written
// individually compose into the whole, and writing one member leaves the
// others as they were (LRM 7.2.1).
module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } pair_t;

  typedef struct packed {
    logic [7:0] x;
    logic [7:0] y;
    logic [7:0] z;
  } triple_t;

  typedef struct packed {
    logic [63:0] high;
    logic [63:0] low;
  } wide_t;

  logic [7:0] read_a;
  logic [7:0] read_b;
  logic [31:0] member_sum;
  logic [15:0] built_whole;
  logic [15:0] after_member_write;
  logic [7:0] triple_x;
  logic [7:0] triple_y;
  logic [7:0] triple_z;
  logic [23:0] triple_whole;
  logic [7:0] copied_a;
  logic [7:0] copied_b;
  logic [15:0] copied_whole;
  logic [63:0] wide_high;
  logic [63:0] wide_low;

  initial begin
    pair_t from_whole;
    pair_t built;
    pair_t partly_cleared;
    pair_t source;
    pair_t copy;
    triple_t triple;
    wide_t wide;

    from_whole = 16'hAABB;
    read_a = from_whole.a;
    read_b = from_whole.b;
    member_sum = from_whole.a + from_whole.b;

    built.a = 8'hAA;
    built.b = 8'hBB;
    built_whole = built;

    partly_cleared = 16'hFFFF;
    partly_cleared.a = 8'h00;
    after_member_write = partly_cleared;

    triple.x = 8'h12;
    triple.y = 8'h34;
    triple.z = 8'h56;
    triple_x = triple.x;
    triple_y = triple.y;
    triple_z = triple.z;
    triple_whole = triple;

    source.a = 8'hAA;
    source.b = 8'hBB;
    copy = source;
    copied_a = copy.a;
    copied_b = copy.b;
    copied_whole = copy;

    wide.high = 64'h0123456789ABCDEF;
    wide.low = 64'hFEDCBA9876543210;
    wide_high = wide.high;
    wide_low = wide.low;
  end

  final begin
    if (read_a !== 8'hAA) $fatal(1, "read_a was %h, expected aa", read_a);
    if (read_b !== 8'hBB) $fatal(1, "read_b was %h, expected bb", read_b);
    if (member_sum !== 32'h165)
      $fatal(1, "member_sum was %h, expected 165", member_sum);
    if (built_whole !== 16'hAABB)
      $fatal(1, "built_whole was %h, expected aabb", built_whole);
    if (after_member_write !== 16'h00FF)
      $fatal(1, "after_member_write was %h, expected 00ff",
             after_member_write);
    if (triple_x !== 8'h12) $fatal(1, "triple_x was %h, expected 12", triple_x);
    if (triple_y !== 8'h34) $fatal(1, "triple_y was %h, expected 34", triple_y);
    if (triple_z !== 8'h56) $fatal(1, "triple_z was %h, expected 56", triple_z);
    if (triple_whole !== 24'h123456)
      $fatal(1, "triple_whole was %h, expected 123456", triple_whole);
    if (copied_a !== 8'hAA)
      $fatal(1, "copied_a was %h, expected aa", copied_a);
    if (copied_b !== 8'hBB)
      $fatal(1, "copied_b was %h, expected bb", copied_b);
    if (copied_whole !== 16'hAABB)
      $fatal(1, "copied_whole was %h, expected aabb", copied_whole);
    if (wide_high !== 64'h0123456789ABCDEF)
      $fatal(1, "wide_high was %h, expected 0123456789abcdef", wide_high);
    if (wide_low !== 64'hFEDCBA9876543210)
      $fatal(1, "wide_low was %h, expected fedcba9876543210", wide_low);
    $display("All checks passed");
  end
endmodule

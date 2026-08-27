// One or more bits of a packed structure can be selected as if the structure
// were a packed array with the range [n-1:0], where n is its width, so a bit
// select or a part-select addresses it by position and pays no attention to
// where the member boundaries fall. A member reached by name is a vector in
// its own right and is selected by its own declared range (LRM 7.2.1,
// 11.5.1).
module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } pair_t;

  logic bit_lsb;
  logic bit_four;
  logic bit_eight;
  logic bit_msb;
  logic [7:0] low_byte;
  logic [7:0] high_byte;
  logic [7:0] low_indexed;
  logic [7:0] high_indexed;
  logic [11:0] across_member_boundary;
  logic [3:0] member_upper;
  logic [3:0] member_lower;
  logic member_bit_high;
  logic member_bit_low;

  initial begin
    pair_t s;

    s = 16'hABCD;
    bit_lsb = s[0];
    bit_four = s[4];
    bit_eight = s[8];
    bit_msb = s[15];
    low_byte = s[7:0];
    high_byte = s[15:8];
    low_indexed = s[0+:8];
    high_indexed = s[15-:8];
    across_member_boundary = s[13:2];

    s = 16'hF00F;
    member_upper = s.a[7:4];
    member_lower = s.a[3:0];
    member_bit_high = s.b[7];
    member_bit_low = s.b[0];
  end

  final begin
    if (bit_lsb !== 1'b1) $fatal(1, "bit_lsb was %b, expected 1", bit_lsb);
    if (bit_four !== 1'b0) $fatal(1, "bit_four was %b, expected 0", bit_four);
    if (bit_eight !== 1'b1)
      $fatal(1, "bit_eight was %b, expected 1", bit_eight);
    if (bit_msb !== 1'b1) $fatal(1, "bit_msb was %b, expected 1", bit_msb);
    if (low_byte !== 8'hCD)
      $fatal(1, "low_byte was %h, expected cd", low_byte);
    if (high_byte !== 8'hAB)
      $fatal(1, "high_byte was %h, expected ab", high_byte);
    if (low_indexed !== 8'hCD)
      $fatal(1, "low_indexed was %h, expected cd", low_indexed);
    if (high_indexed !== 8'hAB)
      $fatal(1, "high_indexed was %h, expected ab", high_indexed);
    if (across_member_boundary !== 12'hAF3)
      $fatal(1, "across_member_boundary was %h, expected af3",
             across_member_boundary);
    if (member_upper !== 4'hF)
      $fatal(1, "member_upper was %h, expected f", member_upper);
    if (member_lower !== 4'h0)
      $fatal(1, "member_lower was %h, expected 0", member_lower);
    if (member_bit_high !== 1'b0)
      $fatal(1, "member_bit_high was %b, expected 0", member_bit_high);
    if (member_bit_low !== 1'b1)
      $fatal(1, "member_bit_low was %b, expected 1", member_bit_low);
    $display("All checks passed");
  end
endmodule

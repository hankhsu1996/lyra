// One or more bits of a packed union can be selected as if the union were a
// packed array with the range [n-1:0], where n is the number of bits in the
// union. A bit select or part-select therefore addresses the union by
// position, reading whatever was last written through any member (LRM 7.3.1,
// 11.5.1).
module Top;
  typedef struct packed {
    logic [7:0] high;
    logic [7:0] low;
  } pair_t;

  typedef union packed {
    pair_t pair;
    logic [15:0] word;
  } combo_t;

  logic bit_lsb;
  logic bit_four;
  logic bit_eight;
  logic bit_msb;
  logic [3:0] low_nibble;
  logic [7:0] mid_byte;
  logic [3:0] high_nibble;
  logic [7:0] after_member_write;

  initial begin
    combo_t u;

    u.word = 16'hABCD;
    bit_lsb = u[0];
    bit_four = u[4];
    bit_eight = u[8];
    bit_msb = u[15];
    low_nibble = u[3:0];
    mid_byte = u[11:4];
    high_nibble = u[15:12];

    u.pair.high = 8'h12;
    after_member_write = u[8+:8];
  end

  final begin
    if (bit_lsb !== 1'b1) $fatal(1, "bit_lsb was %b, expected 1", bit_lsb);
    if (bit_four !== 1'b0) $fatal(1, "bit_four was %b, expected 0", bit_four);
    if (bit_eight !== 1'b1)
      $fatal(1, "bit_eight was %b, expected 1", bit_eight);
    if (bit_msb !== 1'b1) $fatal(1, "bit_msb was %b, expected 1", bit_msb);
    if (low_nibble !== 4'hD)
      $fatal(1, "low_nibble was %h, expected d", low_nibble);
    if (mid_byte !== 8'hBC)
      $fatal(1, "mid_byte was %h, expected bc", mid_byte);
    if (high_nibble !== 4'hA)
      $fatal(1, "high_nibble was %h, expected a", high_nibble);
    if (after_member_write !== 8'h12)
      $fatal(1, "after_member_write was %h, expected 12", after_member_write);
    $display("All checks passed");
  end
endmodule

// A part-select that runs past the end of its vector reads x at the
// positions that do not exist and the stored bits at those that do, and when
// it is written it changes only the positions that do exist. One that lies
// entirely outside the vector reads as x throughout and writes nothing. A
// 4-state result carries those x bits; a 2-state one reads them as 0. A
// part-select may be wider than the vector it is taken in, and then stores
// the bits of what is assigned that fall on positions that exist. A select
// whose position is constant does the same as one known only at run time,
// and a part a hundred bits wide of a vector three hundred bits long does the
// same as a narrow one (LRM 11.5.1, 11.3.4).
module Top;
  bit [7:0] two_state_source;
  logic [7:0] four_state_source;
  int idx;
  bit [3:0] high_2state;
  bit [3:0] low_2state;
  bit [3:0] outside_2state;
  logic [3:0] high_4state;
  logic [3:0] low_4state;
  logic [3:0] outside_4state;
  bit [7:0] high_2state_write;
  bit [7:0] low_2state_write;
  bit [7:0] outside_2state_write;
  logic [7:0] high_4state_write;
  logic [7:0] low_4state_write;
  logic [7:0] outside_4state_write;
  logic [7:0] covering_write;
  logic [7:0] wider_from_below_write;
  logic [7:0] local_covering_write;
  logic [7:0] local_wider_from_below_write;
  logic [5:0] constant_high_read;
  logic [3:0] constant_outside_read;
  logic [7:0] constant_high_write;
  logic [7:0] constant_outside_write;
  logic [299:0] long_source;
  bit [299:0] long_two_state_source;
  logic [99:0] long_part;
  int long_mismatches;
  logic [99:0] long_read_at_unknown;
  logic [299:0] long_written_at_unknown;

  function automatic logic [7:0] write_wider_to_a_local(
      logic [7:0] start, int at, logic [69:0] bits);
    logic [7:0] f;
    f = start;
    f[at +: 70] = bits;
    return f;
  endfunction

  initial begin
    outside_2state = 4'hF;
    high_4state = 4'h0;
    low_4state = 4'h0;
    outside_4state = 4'h0;

    two_state_source = 8'b0110_1001;
    four_state_source = 8'b0110_1001;

    idx = 6;
    high_2state = two_state_source[idx +: 4];
    high_4state = four_state_source[idx +: 4];
    idx = -2;
    low_2state = two_state_source[idx +: 4];
    low_4state = four_state_source[idx +: 4];
    idx = 100;
    outside_2state = two_state_source[idx +: 4];
    outside_4state = four_state_source[idx +: 4];

    high_2state_write = 8'h0F;
    high_4state_write = 8'h0F;
    idx = 6;
    high_2state_write[idx +: 4] = 4'b1001;
    high_4state_write[idx +: 4] = 4'b1001;

    low_2state_write = 8'hF0;
    low_4state_write = 8'hF0;
    idx = -2;
    low_2state_write[idx +: 4] = 4'b1001;
    low_4state_write[idx +: 4] = 4'b1001;

    outside_2state_write = 8'hA5;
    outside_4state_write = 8'hA5;
    idx = 100;
    outside_2state_write[idx +: 4] = 4'b1001;
    outside_4state_write[idx +: 4] = 4'b1001;

    covering_write = 8'h00;
    idx = -3;
    covering_write[idx +: 70] = {70{1'b1}};

    wider_from_below_write = 8'hF0;
    idx = -66;
    wider_from_below_write[idx +: 70] = {4'b0101, 66'd0};

    local_covering_write = write_wider_to_a_local(8'h00, -3, {70{1'b1}});
    local_wider_from_below_write =
        write_wider_to_a_local(8'hF0, -66, {4'b0101, 66'd0});

    constant_high_read = four_state_source[11:6];
    constant_outside_read = four_state_source[34:31];
    constant_high_write = 8'h0F;
    constant_high_write[9:6] = 4'b1001;
    constant_outside_write = 8'hA5;
    constant_outside_write[34:31] = 4'b1001;
    constant_outside_write[12] = 1'b0;

    long_source = {10{30'h2A5C_3F1E}};
    long_source[17] = 1'bx;
    long_source[201] = 1'bz;
    long_two_state_source = {10{30'h1B3C_5D7E}};
    long_part = {25{4'b10xz}};
    long_mismatches = 0;
    for (int at = -150; at < 330; at += 23) begin
      logic [99:0] got;
      bit [99:0] got_two_state;
      logic [299:0] written;
      bit [299:0] written_two_state;
      got = long_source[at +: 100];
      got_two_state = long_two_state_source[at +: 100];
      for (int t = 0; t < 100; t++) begin
        bit exists;
        exists = at + t >= 0 && at + t < 300;
        if (got[t] !== (exists ? long_source[at + t] : 1'bx)) long_mismatches++;
        if (got_two_state[t] !== (exists ? long_two_state_source[at + t] : 1'b0))
          long_mismatches++;
      end
      written = '1;
      written_two_state = '1;
      written[at +: 100] = long_part;
      written_two_state[at +: 100] = long_part;
      for (int q = 0; q < 300; q++) begin
        logic placed;
        placed = (q >= at && q < at + 100) ? long_part[q - at] : 1'b1;
        if (written[q] !== placed) long_mismatches++;
        if (written_two_state[q] !== (placed === 1'b1)) long_mismatches++;
      end
    end
    begin
      integer unknown_at;
      unknown_at = 'x;
      long_read_at_unknown = long_source[unknown_at +: 100];
      long_written_at_unknown = '0;
      long_written_at_unknown[unknown_at +: 100] = long_part;
    end
  end

  final begin
    if (high_2state !== 4'b0001)
      $fatal(1, "2-state read at 6 was %b, expected 0001", high_2state);
    if (low_2state !== 4'b0100)
      $fatal(1, "2-state read at -2 was %b, expected 0100", low_2state);
    if (outside_2state !== 4'b0000)
      $fatal(1, "2-state read at 100 was %b, expected 0000", outside_2state);
    if (high_4state !== 4'bxx01)
      $fatal(1, "4-state read at 6 was %b, expected xx01", high_4state);
    if (low_4state !== 4'b01xx)
      $fatal(1, "4-state read at -2 was %b, expected 01xx", low_4state);
    if (outside_4state !== 4'bxxxx)
      $fatal(1, "4-state read at 100 was %b, expected xxxx", outside_4state);
    if (high_2state_write !== 8'b0100_1111)
      $fatal(1, "2-state write at 6 gave %b, expected 01001111",
             high_2state_write);
    if (low_2state_write !== 8'b1111_0010)
      $fatal(1, "2-state write at -2 gave %b, expected 11110010",
             low_2state_write);
    if (outside_2state_write !== 8'hA5)
      $fatal(1, "2-state write at 100 gave %h, expected a5",
             outside_2state_write);
    if (high_4state_write !== 8'b0100_1111)
      $fatal(1, "4-state write at 6 gave %b, expected 01001111",
             high_4state_write);
    if (low_4state_write !== 8'b1111_0010)
      $fatal(1, "4-state write at -2 gave %b, expected 11110010",
             low_4state_write);
    if (outside_4state_write !== 8'hA5)
      $fatal(1, "4-state write at 100 gave %h, expected a5",
             outside_4state_write);
    if (covering_write !== 8'hFF)
      $fatal(1, "a 70-bit write covering the vector gave %h, expected ff",
             covering_write);
    if (wider_from_below_write !== 8'hF5)
      $fatal(1, "a 70-bit write ending at bit 3 gave %h, expected f5",
             wider_from_below_write);
    if (local_covering_write !== 8'hFF)
      $fatal(1, "a 70-bit write covering a local gave %h, expected ff",
             local_covering_write);
    if (local_wider_from_below_write !== 8'hF5)
      $fatal(1, "a 70-bit write ending at a local's bit 3 gave %h, expected f5",
             local_wider_from_below_write);
    if (constant_high_read !== 6'bxxxx01)
      $fatal(1, "a read of [11:6] was %b, expected xxxx01", constant_high_read);
    if (constant_outside_read !== 4'bxxxx)
      $fatal(1, "a read of [34:31] was %b, expected xxxx",
             constant_outside_read);
    if (constant_high_write !== 8'b0100_1111)
      $fatal(1, "a write of [9:6] gave %b, expected 01001111",
             constant_high_write);
    if (constant_outside_write !== 8'hA5)
      $fatal(1, "writes of [34:31] and [12] gave %h, expected a5",
             constant_outside_write);
    if (long_mismatches != 0)
      $fatal(1, "%0d positions of 100-bit parts of a 300-bit vector were wrong",
             long_mismatches);
    if (long_read_at_unknown !== {100{1'bx}})
      $fatal(1, "a 100-bit read at an unknown index gave %h",
             long_read_at_unknown);
    if (long_written_at_unknown !== '0)
      $fatal(1, "a 100-bit write at an unknown index gave %h",
             long_written_at_unknown);
    $display("All checks passed");
  end
endmodule

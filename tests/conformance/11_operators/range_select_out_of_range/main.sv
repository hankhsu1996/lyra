// A part-select that runs past the end of its vector reads x at the
// positions that do not exist and the stored bits at those that do, and when
// it is written it changes only the positions that do exist. One that lies
// entirely outside the vector reads as x throughout and writes nothing. A
// 4-state result carries those x bits; a 2-state one reads them as 0. A
// part-select may be wider than the vector it is taken in, and then stores
// the bits of what is assigned that fall on positions that exist
// (LRM 11.5.1, 11.3.4).
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
    $display("All checks passed");
  end
endmodule

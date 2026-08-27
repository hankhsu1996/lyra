// A bit-select whose address is invalid -- past either end of the vector, or
// holding one or more x or z bits -- returns x from a 4-state vector and 0
// from a 2-state one, whatever the vector holds at the positions that do
// exist. An address is invalid as soon as any single bit of it is x or z. A
// bit-select addresses one bit, so an invalid address names no bit the vector
// has and a write through it leaves every bit of the vector as it stood
// (LRM 11.5.1).
module Top;
  bit [7:0] two_state;
  logic [7:0] four_state;
  integer idx;
  bit above_2state;
  bit negative_2state;
  bit in_range_2state;
  logic above_4state;
  logic negative_4state;
  logic x_index_4state;
  logic z_index_4state;
  logic partly_unknown_4state;
  logic in_range_4state;
  bit [7:0] two_state_ones;
  bit [7:0] two_state_zeros;
  logic [7:0] four_state_ones;
  logic [7:0] four_state_zeros;
  bit [7:0] in_range_write;

  initial begin
    above_2state = 1'b1;
    negative_2state = 1'b1;
    above_4state = 1'b0;
    negative_4state = 1'b0;
    x_index_4state = 1'b0;
    z_index_4state = 1'b0;
    partly_unknown_4state = 1'b0;

    two_state = 8'b1111_1111;
    four_state = 8'b1111_1111;

    idx = 8;
    above_2state = two_state[idx];
    idx = -1;
    negative_2state = two_state[idx];
    idx = 3;
    in_range_2state = two_state[idx];

    idx = 8;
    above_4state = four_state[idx];
    idx = -1;
    negative_4state = four_state[idx];
    idx = 'x;
    x_index_4state = four_state[idx];
    idx = 'z;
    z_index_4state = four_state[idx];
    idx = 32'b0000_0000_0000_0000_0000_0000_0000_001x;
    partly_unknown_4state = four_state[idx];
    idx = 3;
    in_range_4state = four_state[idx];

    two_state_ones = 8'hFF;
    two_state_zeros = 8'h00;
    four_state_ones = 8'hFF;
    four_state_zeros = 8'h00;
    in_range_write = 8'hFF;

    idx = 8;
    two_state_ones[idx] = 1'b0;
    two_state_zeros[idx] = 1'b1;
    four_state_ones[idx] = 1'b0;
    four_state_zeros[idx] = 1'b1;
    idx = -1;
    two_state_ones[idx] = 1'b0;
    two_state_zeros[idx] = 1'b1;
    four_state_ones[idx] = 1'b0;
    four_state_zeros[idx] = 1'b1;
    idx = 'x;
    two_state_ones[idx] = 1'b0;
    two_state_zeros[idx] = 1'b1;
    four_state_ones[idx] = 1'b0;
    four_state_zeros[idx] = 1'b1;
    idx = 'z;
    two_state_ones[idx] = 1'b0;
    two_state_zeros[idx] = 1'b1;
    four_state_ones[idx] = 1'b0;
    four_state_zeros[idx] = 1'b1;

    idx = 3;
    in_range_write[idx] = 1'b0;
  end

  final begin
    if (above_2state !== 1'b0)
      $fatal(1, "two_state[8] was %b, expected 0", above_2state);
    if (negative_2state !== 1'b0)
      $fatal(1, "two_state[-1] was %b, expected 0", negative_2state);
    if (in_range_2state !== 1'b1)
      $fatal(1, "two_state[3] was %b, expected 1", in_range_2state);
    if (above_4state !== 1'bx)
      $fatal(1, "four_state[8] was %b, expected x", above_4state);
    if (negative_4state !== 1'bx)
      $fatal(1, "four_state[-1] was %b, expected x", negative_4state);
    if (x_index_4state !== 1'bx)
      $fatal(1, "four_state[x] was %b, expected x", x_index_4state);
    if (z_index_4state !== 1'bx)
      $fatal(1, "four_state[z] was %b, expected x", z_index_4state);
    if (partly_unknown_4state !== 1'bx)
      $fatal(1, "four_state[2 with an x bit] was %b, expected x",
             partly_unknown_4state);
    if (in_range_4state !== 1'b1)
      $fatal(1, "four_state[3] was %b, expected 1", in_range_4state);
    if (two_state_ones !== 8'hFF)
      $fatal(1, "invalid writes of 0 into a 2-state ff gave %h, expected ff",
             two_state_ones);
    if (two_state_zeros !== 8'h00)
      $fatal(1, "invalid writes of 1 into a 2-state 00 gave %h, expected 00",
             two_state_zeros);
    if (four_state_ones !== 8'hFF)
      $fatal(1, "invalid writes of 0 into a 4-state ff gave %h, expected ff",
             four_state_ones);
    if (four_state_zeros !== 8'h00)
      $fatal(1, "invalid writes of 1 into a 4-state 00 gave %h, expected 00",
             four_state_zeros);
    if (in_range_write !== 8'hF7)
      $fatal(1, "a write of 0 at 3 gave %h, expected f7", in_range_write);
    $display("All checks passed");
  end
endmodule

// If a packed union holds both a two-state member and a four-state member,
// the whole union is four-state. Reading the two-state member converts the
// shared bits from four-state, so an unknown or high-impedance bit reads as
// zero, while reading the four-state member shows those bits as they were
// stored. Writing the two-state member converts the other way and leaves the
// union holding only known bits (LRM 6.11.2, 7.3.1).
module Top;
  typedef union packed {
    logic [15:0] as_logic;
    bit [15:0] as_bit;
  } pun_t;

  logic [15:0] two_state_read;
  logic [15:0] four_state_read;
  logic [15:0] after_two_state_write;

  initial begin
    pun_t u;

    // Unknown and high-impedance bits in the low nibbles, so a read that
    // failed to convert them would differ from one that did.
    u.as_logic = 16'b11011110_1010xxzz;
    two_state_read = u.as_bit;
    four_state_read = u.as_logic;

    u.as_bit = 16'hBEEF;
    after_two_state_write = u.as_logic;
  end

  final begin
    if (two_state_read !== 16'hDEA0)
      $fatal(1, "two_state_read was %b, expected 1101111010100000",
             two_state_read);
    if (four_state_read !== 16'b11011110_1010xxzz)
      $fatal(1, "four_state_read was %b, expected 110111101010xxzz",
             four_state_read);
    if (after_two_state_write !== 16'hBEEF)
      $fatal(1, "after_two_state_write was %b, expected 1011111011101111",
             after_two_state_write);
    $display("All checks passed");
  end
endmodule

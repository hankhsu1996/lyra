// When a four-state value is automatically converted to a two-state value,
// every unknown and high-impedance bit becomes zero and every known bit is
// kept. The conversion applies to the whole value the assignment produces,
// so bits that extension added are converted along with the rest: a sign bit
// of x extends as x and then collapses to zero, while a sign bit of 1 extends
// as 1 and survives. Which bits collapse is decided per bit and not by how
// many there are, so the rule reads the same over a vector of any width
// (LRM 6.11.2).
module Top;
  bit [3:0] equal_width;
  bit [7:0] widened_unsigned;
  bit [7:0] widened_unknown_sign;
  bit [7:0] widened_known_sign;
  bit [127:0] wide_equal_width;
  bit [127:0] wide_widened_unknown_sign;
  bit [95:0] wide_widened_known_sign;

  initial begin
    logic [3:0] four_state;
    logic signed [3:0] four_state_signed;
    logic [127:0] wide_four_state;
    logic signed [64:0] wide_four_state_unknown_sign;
    logic signed [32:0] wide_four_state_known_sign;

    four_state = 4'b10xz;
    equal_width = four_state;
    widened_unsigned = four_state;

    four_state_signed = 4'bx010;
    widened_unknown_sign = four_state_signed;
    four_state_signed = 4'b1010;
    widened_known_sign = four_state_signed;

    // A known bit sits between the unknown ones, so a conversion that cleared
    // too much and one that cleared too little give different answers.
    wide_four_state = {60'h0, 4'bx1z0, 64'hFFFF_FFFF_FFFF_FFFF};
    wide_equal_width = wide_four_state;

    wide_four_state_unknown_sign = {1'bx, 64'hA5A5_A5A5_A5A5_A5A5};
    wide_widened_unknown_sign = wide_four_state_unknown_sign;

    wide_four_state_known_sign = 33'h1_0000_0001;
    wide_widened_known_sign = wide_four_state_known_sign;
  end

  final begin
    if (equal_width !== 4'b1000)
      $fatal(1, "equal_width was %b, expected 1000", equal_width);
    if (widened_unsigned !== 8'b00001000)
      $fatal(1, "widened_unsigned was %b, expected 00001000",
             widened_unsigned);
    if (widened_unknown_sign !== 8'b00000010)
      $fatal(1, "widened_unknown_sign was %b, expected 00000010",
             widened_unknown_sign);
    if (widened_known_sign !== 8'b11111010)
      $fatal(1, "widened_known_sign was %b, expected 11111010",
             widened_known_sign);
    if (wide_equal_width !== 128'h0000_0000_0000_0004_FFFF_FFFF_FFFF_FFFF)
      $fatal(1, "wide_equal_width was %h, expected 0000000000000004ffffffffffffffff",
             wide_equal_width);
    if (wide_widened_unknown_sign !==
        128'h0000_0000_0000_0000_A5A5_A5A5_A5A5_A5A5)
      $fatal(1, "wide_widened_unknown_sign was %h, expected 0000000000000000a5a5a5a5a5a5a5a5",
             wide_widened_unknown_sign);
    if (wide_widened_known_sign !== 96'hFFFF_FFFF_FFFF_FFFF_0000_0001)
      $fatal(1, "wide_widened_known_sign was %h, expected ffffffffffffffff00000001",
             wide_widened_known_sign);
    $display("All checks passed");
  end
endmodule

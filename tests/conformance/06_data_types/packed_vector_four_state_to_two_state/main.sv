// When a four-state value is automatically converted to a two-state value,
// every unknown and high-impedance bit becomes zero and every known bit is
// kept. The conversion applies to the whole value the assignment produces,
// so bits that extension added are converted along with the rest: a sign bit
// of x extends as x and then collapses to zero, while a sign bit of 1 extends
// as 1 and survives (LRM 6.11.2).
module Top;
  bit [3:0] equal_width;
  bit [7:0] widened_unsigned;
  bit [7:0] widened_unknown_sign;
  bit [7:0] widened_known_sign;

  initial begin
    logic [3:0] four_state;
    logic signed [3:0] four_state_signed;

    four_state = 4'b10xz;
    equal_width = four_state;
    widened_unsigned = four_state;

    four_state_signed = 4'bx010;
    widened_unknown_sign = four_state_signed;
    four_state_signed = 4'b1010;
    widened_known_sign = four_state_signed;
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
    $display("All checks passed");
  end
endmodule

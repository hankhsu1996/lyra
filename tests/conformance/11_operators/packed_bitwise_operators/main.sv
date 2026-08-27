// The binary bitwise operators & | ^ and ~^ combine each bit of one operand
// with the bit in the same position of the other, and the unary ~ negates
// every bit of a single operand. An x or a z propagates as x wherever the
// other bit does not settle the result on its own, so a 0 under & and a 1
// under | stay known. Operands of unequal width are first brought to the
// wider width, the narrower one sign-extended when both operands are signed
// and zero-extended when either is unsigned (LRM 11.4.8, Tables 11-11 to
// 11-15).
module Top;
  bit [3:0] two_state_not;
  bit [3:0] two_state_and;
  bit [3:0] two_state_or;
  bit [3:0] two_state_xor;
  bit [3:0] two_state_xnor;
  logic [3:0] four_state_not;
  logic [3:0] four_state_and;
  logic [3:0] four_state_or;
  logic [3:0] four_state_xor;
  logic [3:0] four_state_xnor;
  logic [3:0] high_impedance_and;
  bit [4:0] odd_width_not;
  logic [7:0] both_signed_and;

  logic [7:0] mixed_sign_and;

  initial begin
    bit [3:0] p;
    bit [3:0] q;
    logic [3:0] r;
    logic [3:0] s;
    reg [3:0] t;
    bit [4:0] odd_width;
    logic signed [7:0] wide_signed;
    logic signed [3:0] narrow_signed;

    logic [7:0] wide_unsigned;

    p = 4'b1010;
    q = 4'b1100;
    two_state_not = ~p;
    two_state_and = p & q;
    two_state_or = p | q;
    two_state_xor = p ^ q;
    two_state_xnor = p ~^ q;

    r = 4'b10xz;
    s = 4'b1100;
    four_state_not = ~r;
    four_state_and = r & s;
    four_state_or = r | s;
    four_state_xor = r ^ s;
    four_state_xnor = r ~^ s;

    t = 4'bz0x1;
    high_impedance_and = t & 4'b1011;

    odd_width = 5'b10101;
    odd_width_not = ~odd_width;

    // The narrow operand's sign bit is 1, so sign extension puts ones above
    // it where zero extension would put zeros.
    narrow_signed = 4'sb1010;
    wide_signed = 8'sb11110000;
    both_signed_and = wide_signed & narrow_signed;

    wide_unsigned = 8'b11110000;
    mixed_sign_and = wide_unsigned & narrow_signed;
  end

  final begin
    if (two_state_not !== 4'b0101)
      $fatal(1, "two_state_not was %b, expected 0101", two_state_not);
    if (two_state_and !== 4'b1000)
      $fatal(1, "two_state_and was %b, expected 1000", two_state_and);
    if (two_state_or !== 4'b1110)
      $fatal(1, "two_state_or was %b, expected 1110", two_state_or);
    if (two_state_xor !== 4'b0110)
      $fatal(1, "two_state_xor was %b, expected 0110", two_state_xor);
    if (two_state_xnor !== 4'b1001)
      $fatal(1, "two_state_xnor was %b, expected 1001", two_state_xnor);
    if (four_state_not !== 4'b01xx)
      $fatal(1, "four_state_not was %b, expected 01xx", four_state_not);
    if (four_state_and !== 4'b1000)
      $fatal(1, "four_state_and was %b, expected 1000", four_state_and);
    if (four_state_or !== 4'b11xx)
      $fatal(1, "four_state_or was %b, expected 11xx", four_state_or);
    if (four_state_xor !== 4'b01xx)
      $fatal(1, "four_state_xor was %b, expected 01xx", four_state_xor);
    if (four_state_xnor !== 4'b10xx)
      $fatal(1, "four_state_xnor was %b, expected 10xx", four_state_xnor);
    if (high_impedance_and !== 4'bx0x1)
      $fatal(1, "high_impedance_and was %b, expected x0x1",
             high_impedance_and);
    if (odd_width_not !== 5'b01010)
      $fatal(1, "odd_width_not was %b, expected 01010", odd_width_not);
    if (both_signed_and !== 8'b11110000)
      $fatal(1, "both_signed_and was %b, expected 11110000", both_signed_and);

    if (mixed_sign_and !== 8'b00000000)
      $fatal(1, "mixed_sign_and was %b, expected 00000000", mixed_sign_and);
    $display("All checks passed");
  end
endmodule

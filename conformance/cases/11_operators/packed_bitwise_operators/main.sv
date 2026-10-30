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

  logic [6:0] and_7;
  logic [6:0] or_7;
  logic [6:0] xor_7;
  logic [6:0] not_7;
  logic [32:0] not_33;
  logic [32:0] xor_33;
  logic [32:0] and_unknown_top_33;
  logic [62:0] not_63;
  logic [63:0] or_64;
  logic [63:0] xnor_64;

  // Every position of an operand takes part, whatever the operand's width.
  initial begin
    logic [6:0] a7;
    logic [6:0] b7;
    logic [32:0] a33;
    logic [32:0] b33;
    logic [62:0] a63;
    logic [63:0] a64;
    logic [63:0] b64;

    xnor_64 = '1;

    a7 = 7'b1010101;
    b7 = 7'b1100110;
    and_7 = a7 & b7;
    or_7 = a7 | b7;
    xor_7 = a7 ^ b7;
    not_7 = ~a7;

    a33 = 33'h0_0000_0000;
    not_33 = ~a33;
    a33 = 33'h1_0000_ffff;
    b33 = 33'h1_ffff_0000;
    xor_33 = a33 ^ b33;
    a33[32] = 1'bx;
    and_unknown_top_33 = a33 & b33;

    a63 = 63'h0;
    not_63 = ~a63;

    a64 = 64'hf0f0_f0f0_f0f0_f0f0;
    b64 = 64'h0f0f_0f0f_0f0f_0f0f;
    or_64 = a64 | b64;
    xnor_64 = a64 ~^ b64;
  end

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

    if (and_7 !== 7'b1000100)
      $fatal(1, "and_7 was %b, expected 1000100", and_7);
    if (or_7 !== 7'b1110111) $fatal(1, "or_7 was %b, expected 1110111", or_7);
    if (xor_7 !== 7'b0110011)
      $fatal(1, "xor_7 was %b, expected 0110011", xor_7);
    if (not_7 !== 7'b0101010)
      $fatal(1, "not_7 was %b, expected 0101010", not_7);
    if (not_33 !== 33'h1_ffff_ffff)
      $fatal(1, "not_33 was %h, expected 1ffffffff", not_33);
    if (xor_33 !== 33'h0_ffff_ffff)
      $fatal(1, "xor_33 was %h, expected 0ffffffff", xor_33);
    if (and_unknown_top_33 !== 33'bx_0000_0000_0000_0000_0000_0000_0000_0000)
      $fatal(1, "and_unknown_top_33 was %b, expected x above 32 zeros",
             and_unknown_top_33);
    if (not_63 !== 63'h7fff_ffff_ffff_ffff)
      $fatal(1, "not_63 was %h, expected 7fffffffffffffff", not_63);
    if (or_64 !== 64'hffff_ffff_ffff_ffff)
      $fatal(1, "or_64 was %h, expected ffffffffffffffff", or_64);
    if (xnor_64 !== 64'h0) $fatal(1, "xnor_64 was %h, expected 0", xnor_64);
    $display("All checks passed");
  end
endmodule

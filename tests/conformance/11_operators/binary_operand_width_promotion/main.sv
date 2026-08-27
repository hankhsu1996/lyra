// A binary operator evaluates in the width of its widest operand, and in an
// assignment the left-hand side counts among those operands, so a bit that
// does not fit that width is lost before any further operator sees it. The
// narrower operand is brought up to the chosen width first: zero-extended
// when either operand is unsigned and sign-extended only when both are
// signed. A comparison sizes its operands the same way but yields one
// unsigned bit, and a comparison with an unsigned operand is an unsigned
// comparison however the other operand is declared (LRM 11.6, 11.6.1,
// Table 11-21, 11.4.4, 11.8.1, 11.8.2).
module Top;
  int bit_plus_int;
  longint int_plus_longint;
  longint bit_plus_longint;
  logic [15:0] sum_in_sixteen_bits;
  logic [16:0] sum_in_seventeen_bits;
  logic [15:0] carry_lost_before_shift;
  logic [15:0] carry_kept_before_shift;
  int signed_operand_extended;
  int unsigned_operand_extended;
  bit signed_less;
  bit signed_greater;
  bit unsigned_less;
  bit unsigned_greater;

  initial begin
    bit one_bit;
    int thirty_two_bits;
    longint sixty_four_bits;
    logic [15:0] all_ones;
    logic [15:0] just_one;
    byte negative_byte;
    bit [7:0] unsigned_byte;
    int compared_against;

    signed_greater = 1'b1;
    unsigned_less = 1'b1;

    one_bit = 1;
    thirty_two_bits = 10;
    bit_plus_int = one_bit + thirty_two_bits;

    thirty_two_bits = 100;
    sixty_four_bits = 1000;
    int_plus_longint = thirty_two_bits + sixty_four_bits;
    bit_plus_longint = one_bit + sixty_four_bits;

    // The same addition, evaluated once with a sixteen-bit destination and
    // once with a seventeen-bit one.
    all_ones = 16'hFFFF;
    just_one = 16'h0001;
    sum_in_sixteen_bits = all_ones + just_one;
    sum_in_seventeen_bits = all_ones + just_one;

    // Adding an unsized decimal literal raises the whole expression to the
    // width of an integer, so the carry is still there for the shift.
    carry_lost_before_shift = (all_ones + just_one) >> 1;
    carry_kept_before_shift = (all_ones + just_one + 0) >> 1;

    negative_byte = -1;
    unsigned_byte = 8'hFF;
    signed_operand_extended = negative_byte + 0;
    unsigned_operand_extended = unsigned_byte + 0;

    // The same eight set bits compared against the same number, read once as
    // a signed value and once as an unsigned one.
    compared_against = 100;
    signed_less = (negative_byte < compared_against);
    signed_greater = (negative_byte > compared_against);
    unsigned_less = (unsigned_byte < compared_against);
    unsigned_greater = (unsigned_byte > compared_against);
  end

  final begin
    if (bit_plus_int !== 11)
      $fatal(1, "bit_plus_int was %0d, expected 11", bit_plus_int);
    if (int_plus_longint !== 1100)
      $fatal(1, "int_plus_longint was %0d, expected 1100", int_plus_longint);
    if (bit_plus_longint !== 1001)
      $fatal(1, "bit_plus_longint was %0d, expected 1001", bit_plus_longint);
    if (sum_in_sixteen_bits !== 16'h0000)
      $fatal(1, "sum_in_sixteen_bits was %h, expected 0000",
             sum_in_sixteen_bits);
    if (sum_in_seventeen_bits !== 17'h10000)
      $fatal(1, "sum_in_seventeen_bits was %h, expected 10000",
             sum_in_seventeen_bits);
    if (carry_lost_before_shift !== 16'h0000)
      $fatal(1, "carry_lost_before_shift was %h, expected 0000",
             carry_lost_before_shift);
    if (carry_kept_before_shift !== 16'h8000)
      $fatal(1, "carry_kept_before_shift was %h, expected 8000",
             carry_kept_before_shift);
    if (signed_operand_extended !== -1)
      $fatal(1, "signed_operand_extended was %0d, expected -1",
             signed_operand_extended);
    if (unsigned_operand_extended !== 255)
      $fatal(1, "unsigned_operand_extended was %0d, expected 255",
             unsigned_operand_extended);
    if (signed_less !== 1'b1)
      $fatal(1, "signed_less was %b, expected 1", signed_less);
    if (signed_greater !== 1'b0)
      $fatal(1, "signed_greater was %b, expected 0", signed_greater);
    if (unsigned_less !== 1'b0)
      $fatal(1, "unsigned_less was %b, expected 0", unsigned_less);
    if (unsigned_greater !== 1'b1)
      $fatal(1, "unsigned_greater was %b, expected 1", unsigned_greater);
    $display("All checks passed");
  end
endmodule

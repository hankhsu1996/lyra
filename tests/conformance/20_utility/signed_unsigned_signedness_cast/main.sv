// $signed and $unsigned cast the signedness of an expression but not its type.
// Each evaluates its operand and returns a value of the same size and the same
// bit pattern, differing only in whether that pattern is read as signed -- so
// what changes is how the result extends when it reaches a wider destination
// (LRM 20.5).
module Top;
  logic [3:0] unsigned_nibble;
  logic signed [3:0] signed_nibble;

  logic [7:0] sign_extended;
  logic [7:0] zero_extended;
  int as_negative;
  int as_positive;
  int signed_operand_made_unsigned;
  int width_of_signed;
  int width_of_unsigned;

  initial begin
    unsigned_nibble = 4'b1111;
    signed_nibble = 4'b1111;

    sign_extended = $signed(unsigned_nibble);
    zero_extended = $unsigned(unsigned_nibble);
    as_negative = $signed(unsigned_nibble);
    as_positive = $unsigned(unsigned_nibble);
    signed_operand_made_unsigned = $unsigned(signed_nibble);

    width_of_signed = $bits($signed(unsigned_nibble));
    width_of_unsigned = $bits($unsigned(signed_nibble));
  end

  final begin
    if (sign_extended !== 8'hFF)
      $fatal(1, "$signed widened to %h, expected ff", sign_extended);
    if (zero_extended !== 8'h0F)
      $fatal(1, "$unsigned widened to %h, expected 0f", zero_extended);
    if (as_negative !== -1)
      $fatal(1, "$signed of all four bits set was %0d, expected -1",
             as_negative);
    if (as_positive !== 15)
      $fatal(1, "$unsigned of all four bits set was %0d, expected 15",
             as_positive);
    if (signed_operand_made_unsigned !== 15)
      $fatal(1, "$unsigned of a signed -1 was %0d, expected 15",
             signed_operand_made_unsigned);
    if (width_of_signed !== 4)
      $fatal(1, "$signed changed the size to %0d, expected 4",
             width_of_signed);
    if (width_of_unsigned !== 4)
      $fatal(1, "$unsigned changed the size to %0d, expected 4",
             width_of_unsigned);
    $display("All checks passed");
  end
endmodule

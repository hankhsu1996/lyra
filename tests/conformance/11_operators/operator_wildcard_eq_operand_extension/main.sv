// Operands of a wildcard equality of unequal bit length are extended the way
// the logical equality operators extend them: zero-extended to the wider
// operand when either one is unsigned, sign-extended when both are signed.
// The extension happens before the wildcard positions are applied
// (LRM 11.4.6, 11.4.5).
module Top;
  logic both_signed_negative;
  logic both_signed_positive;
  logic one_unsigned_zero_extends;
  logic signed_with_wildcard;
  logic unsigned_narrow_against_int;

  initial begin
    one_unsigned_zero_extends = 1'b1;

    begin
      logic signed [7:0] narrow;
      logic signed [15:0] wide;
      narrow = -1;
      wide = -1;
      both_signed_negative = narrow ==? wide;
    end

    begin
      logic signed [7:0] narrow;
      logic signed [15:0] wide;
      narrow = 8'sd5;
      wide = 16'sd5;
      both_signed_positive = narrow ==? wide;
    end

    begin
      logic [7:0] narrow;
      logic signed [15:0] wide;
      narrow = 8'd255;
      wide = -1;
      one_unsigned_zero_extends = narrow ==? wide;
    end

    begin
      logic signed [7:0] narrow;
      logic signed [15:0] wide;
      narrow = -1;
      wide = 16'sb1111_1111_zzzz_zzzz;
      signed_with_wildcard = narrow ==? wide;
    end

    begin
      logic [3:0] narrow;
      int wide;
      narrow = 4'd5;
      wide = 32'd5;
      unsigned_narrow_against_int = narrow ==? wide;
    end
  end

  final begin
    if (both_signed_negative !== 1'b1)
      $fatal(1, "8-bit -1 ==? 16-bit -1 was %b, expected 1",
             both_signed_negative);
    if (both_signed_positive !== 1'b1)
      $fatal(1, "8-bit 5 ==? 16-bit 5 was %b, expected 1",
             both_signed_positive);
    if (one_unsigned_zero_extends !== 1'b0)
      $fatal(1, "unsigned 255 ==? signed -1 was %b, expected 0",
             one_unsigned_zero_extends);
    if (signed_with_wildcard !== 1'b1)
      $fatal(1, "sign extension under a wildcard low byte was %b, expected 1",
             signed_with_wildcard);
    if (unsigned_narrow_against_int !== 1'b1)
      $fatal(1, "4-bit 5 ==? int 5 was %b, expected 1",
             unsigned_narrow_against_int);
    $display("All checks passed");
  end
endmodule

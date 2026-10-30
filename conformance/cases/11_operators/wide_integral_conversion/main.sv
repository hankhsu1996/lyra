// Assigning between integral types of different widths extends the
// right-hand side to the width of the left, sign-extending if and only if
// the right-hand side is signed, and drops the surplus high bits when the
// left-hand side is narrower. Signedness changes only the interpretation of
// a value, never its bit pattern, so a value that survives both directions
// comes back unchanged (LRM 11.8.3, 11.4.3.1).
module Top;
  bit [127:0] positive_widened;
  bit signed [127:0] negative_widened;
  bit [127:0] unsigned_widened;
  bit [127:0] signed_source_widened;
  int truncated;
  int truncated_signed;
  bit [127:0] longint_widened;
  longint round_trip;

  initial begin
    begin
      int narrow_positive;
      int narrow_negative;
      bit [63:0] unsigned_source;
      longint signed_source;
      bit [127:0] wide_unsigned;
      bit signed [127:0] wide_signed;

      narrow_positive = 32'h12345678;
      positive_widened = narrow_positive;

      narrow_negative = -1;
      negative_widened = narrow_negative;

      unsigned_source = 64'hFFFFFFFFFFFFFFFF;
      unsigned_widened = unsigned_source;

      signed_source = -1;
      signed_source_widened = signed_source;

      wide_unsigned = 128'hDEADBEEF_12345678;
      truncated = wide_unsigned;

      wide_signed = -1;
      truncated_signed = wide_signed;

      signed_source = 64'h123456789ABCDEF0;
      longint_widened = signed_source;
      round_trip = longint_widened;
    end
  end

  final begin
    if (positive_widened !== 128'h12345678)
      $fatal(1, "widening a positive int gave %h, expected 12345678",
             positive_widened);
    if (negative_widened !== 128'shFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
      $fatal(1, "widening -1 gave %h, expected all ones", negative_widened);
    if (unsigned_widened !== 128'hFFFFFFFFFFFFFFFF)
      $fatal(1, "widening an unsigned 64-bit value gave %h, expected zeros",
             unsigned_widened);
    if (signed_source_widened !== 128'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
      $fatal(1, "widening a signed -1 gave %h, expected all ones",
             signed_source_widened);
    if (truncated !== 32'h12345678)
      $fatal(1, "narrowing a 128-bit value gave %h, expected 12345678",
             truncated);
    if (truncated_signed !== -1)
      $fatal(1, "narrowing a 128-bit -1 gave %0d, expected -1",
             truncated_signed);
    if (longint_widened !== 128'h123456789ABCDEF0)
      $fatal(1, "widening a longint gave %h, expected 123456789abcdef0",
             longint_widened);
    if (round_trip !== 64'h123456789ABCDEF0)
      $fatal(1, "the round trip gave %h, expected 123456789abcdef0",
             round_trip);
    $display("All checks passed");
  end
endmodule

// Assigning between integral types of different widths extends the
// right-hand side to the width of the left, sign-extending if and only if
// the right-hand side is signed, and drops the surplus high bits when the
// left-hand side is narrower. Signedness changes only the interpretation of
// a value, never its bit pattern, so a value that survives both directions
// comes back unchanged. None of this depends on how wide the value is: one
// of five thousand bits narrows to its low bits and widens with zeros above
// it as a narrow one does (LRM 11.8.3, 11.4.3.1).
module Top;
  bit [127:0] positive_widened;
  bit signed [127:0] negative_widened;
  bit [127:0] unsigned_widened;
  bit [127:0] signed_source_widened;
  int truncated;
  int truncated_signed;
  bit [127:0] longint_widened;
  longint round_trip;
  logic [99:0] narrowed_from_long;
  int int_from_long;
  bit widened_from_long_kept;
  int repeated_from_long;

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

    begin
      logic [4999:0] long_source;
      logic [4999:0] long_count;
      logic [5999:0] widened_long;
      long_source = {50{100'hF_EDCB_A987_6543_210F_EDCB_A987}};
      long_source[3] = 1'bx;
      narrowed_from_long = long_source;
      int_from_long = long_source;
      widened_long = long_source;
      widened_from_long_kept = widened_long === {1000'b0, long_source};
      long_count = 5000'd3;
      repeated_from_long = 0;
      repeat (long_count) repeated_from_long++;
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
    if (narrowed_from_long !== {96'hF_EDCB_A987_6543_210F_EDCB_A98, 4'bx111})
      $fatal(1, "narrowing a 5000-bit value gave %h", narrowed_from_long);
    if (int_from_long !== 32'hEDCBA987)
      $fatal(1, "a 5000-bit value as an int gave %h", int_from_long);
    if (!widened_from_long_kept)
      $fatal(1, "widening a 5000-bit value changed its bits or filled ones");
    if (repeated_from_long !== 3)
      $fatal(1, "a repeat counted by a 5000-bit 3 ran %0d times",
             repeated_from_long);
    $display("All checks passed");
  end
endmodule

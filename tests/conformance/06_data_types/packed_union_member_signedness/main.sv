// Members of a packed union view the same bits, and each member is read with
// the signedness its own declaration gives it. The same stored bits therefore
// read as a large positive value through an unsigned member and as a negative
// value through a signed one, and only the signed member's value compares as
// negative (LRM 6.11.3, 7.3.1).
module Top;
  typedef union packed {
    logic        [7:0] as_unsigned;
    logic signed [7:0] as_signed;
  } byte_view_t;

  logic signed [31:0] widened_unsigned;
  logic signed [31:0] widened_signed;
  logic unsigned_compares_negative;
  logic signed_compares_negative;

  initial begin
    byte_view_t u;

    u.as_unsigned = 8'hFF;
    widened_unsigned = u.as_unsigned;
    widened_signed = u.as_signed;
    unsigned_compares_negative = (u.as_unsigned < 0);
    signed_compares_negative = (u.as_signed < 0);
  end

  final begin
    if (widened_unsigned !== 255)
      $fatal(1, "widened_unsigned was %0d, expected 255", widened_unsigned);
    if (widened_signed !== -1)
      $fatal(1, "widened_signed was %0d, expected -1", widened_signed);
    if (unsigned_compares_negative !== 1'b0)
      $fatal(1, "unsigned_compares_negative was %b, expected 0",
             unsigned_compares_negative);
    if (signed_compares_negative !== 1'b1)
      $fatal(1, "signed_compares_negative was %b, expected 1",
             signed_compares_negative);
    $display("All checks passed");
  end
endmodule

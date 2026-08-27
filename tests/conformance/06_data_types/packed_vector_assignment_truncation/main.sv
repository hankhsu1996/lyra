// When the left-hand side of an assignment is narrower than the right-hand
// side, the most significant bits of the right-hand value are discarded and
// the surviving bits are kept as they stand. Nothing records what was cut, so
// widening the narrowed value again pads it rather than restoring it, and
// truncating away the sign bit of a signed value can change its sign
// (LRM 6.11.2, 10.7).
module Top;
  bit [3:0] truncated;
  bit [7:0] widened_again;
  logic [3:0] truncated_keeps_unknown;
  logic signed [4:0] truncation_changes_sign;

  initial begin
    bit [7:0] wide;
    logic [7:0] wide_four_state;
    logic signed [7:0] wide_signed;

    // The discarded bits are not already zero, so a missing truncation shows.
    wide = 8'b10110110;
    truncated = wide;
    widened_again = truncated;

    wide_four_state = 8'b01101x1z;
    truncated_keeps_unknown = wide_four_state;

    wide_signed = -8'sd113;
    truncation_changes_sign = wide_signed;
  end

  final begin
    if (truncated !== 4'b0110)
      $fatal(1, "truncated was %b, expected 0110", truncated);
    if (widened_again !== 8'b00000110)
      $fatal(1, "widened_again was %b, expected 00000110", widened_again);
    if (truncated_keeps_unknown !== 4'b1x1z)
      $fatal(1, "truncated_keeps_unknown was %b, expected 1x1z",
             truncated_keeps_unknown);
    if (truncation_changes_sign !== 15)
      $fatal(1, "truncation_changes_sign was %0d, expected 15",
             truncation_changes_sign);
    $display("All checks passed");
  end
endmodule

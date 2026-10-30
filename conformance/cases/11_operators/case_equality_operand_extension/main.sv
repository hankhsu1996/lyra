// When the operands of an equality comparison are of unequal bit lengths and
// either of them is unsigned, the shorter one is zero-extended to the width of
// the longer before the bits are compared (LRM 11.4.5). A narrow unsigned value
// therefore equals a wide one holding the same number, including when its own
// most significant bit is set and a sign extension would have carried ones into
// the added positions.
module Top;
  logic [3:0] narrow;
  logic [3:0] high_bit;
  logic same_number;
  logic other_number;
  logic high_bit_same;

  initial begin
    narrow = 4'd5;
    same_number = (narrow === 32'd5);
    other_number = (narrow === 32'd6);

    high_bit = 4'b1111;
    high_bit_same = (high_bit === 32'd15);
  end

  final begin
    if (same_number !== 1'b1)
      $fatal(1, "same_number was %b, expected 1", same_number);
    if (other_number !== 1'b0)
      $fatal(1, "other_number was %b, expected 0", other_number);
    if (high_bit_same !== 1'b1)
      $fatal(1, "high_bit_same was %b, expected 1", high_bit_same);
    $display("All checks passed");
  end
endmodule

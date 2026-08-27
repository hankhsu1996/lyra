// A bit-select result is unsigned regardless of the signedness of its
// operand, and so is a part-select result, even one that spans the whole
// vector. Widening such a select therefore fills with zeros, while widening
// the signed vector itself fills with its sign bit (LRM 11.8.1, 11.5.1).
module Top;
  bit signed [7:0] value;
  int msb_bit;
  int near_msb_bit;
  int high_nibble;
  int low_nibble;
  int whole_vector;
  int no_select;
  bit signed [15:0] widened_bit;
  bit signed [15:0] widened_part;

  initial begin
    near_msb_bit = 7;

    value = 8'sb1000_0001;
    msb_bit = value[7];
    near_msb_bit = value[6];
    high_nibble = value[7:4];
    low_nibble = value[3:0];
    whole_vector = value[7:0];
    no_select = value;
    widened_bit = value[7];
    widened_part = value[7:4];
  end

  final begin
    if (msb_bit !== 1)
      $fatal(1, "value[7] widened to %0d, expected 1", msb_bit);
    if (near_msb_bit !== 0)
      $fatal(1, "value[6] widened to %0d, expected 0", near_msb_bit);
    if (high_nibble !== 8)
      $fatal(1, "value[7:4] widened to %0d, expected 8", high_nibble);
    if (low_nibble !== 1)
      $fatal(1, "value[3:0] widened to %0d, expected 1", low_nibble);
    if (whole_vector !== 129)
      $fatal(1, "value[7:0] widened to %0d, expected 129", whole_vector);
    if (no_select !== -127)
      $fatal(1, "value widened to %0d, expected -127", no_select);
    if (widened_bit !== 16'h0001)
      $fatal(1, "value[7] widened to %h, expected 0001", widened_bit);
    if (widened_part !== 16'h0008)
      $fatal(1, "value[7:4] widened to %h, expected 0008", widened_part);
    $display("All checks passed");
  end
endmodule

// A vector's range specification gives addresses to its bits: the left bound
// names the most significant bit and the right bound the least significant.
// Either bound may be greater than, equal to, or less than the other, and
// either may be negative, so which bit an index reaches depends on the
// declared range. A non-indexed part-select names its more significant bit
// first, and an indexed part-select ascends the bit range with +: and
// descends it with -: (LRM 6.9.1, 7.4.1, 11.5.1).
module Top;
  bit [0:7] ascending_write;
  bit ascending_read;
  bit [-1:6] negative_bound_write;
  bit [0:7] ascending_part_write;
  bit [6:1] descending_offset_write;
  bit [7:0] ascending_plus_colon;
  bit [7:0] ascending_minus_colon;
  bit variable_index_read;

  initial begin
    bit [0:7] source;
    bit [6:1] descending_source;
    integer index;

    ascending_write = 8'h00;
    ascending_write[2] = 1'b1;

    source = 8'b00100000;
    ascending_read = source[2];

    negative_bound_write = 8'h00;
    negative_bound_write[0] = 1'b1;

    ascending_part_write = 8'h00;
    ascending_part_write[3:5] = 3'b111;

    descending_offset_write = 6'h00;
    descending_offset_write[3] = 1'b1;

    source = 8'b10110010;
    ascending_plus_colon = source[1+:3];
    ascending_minus_colon = source[5-:3];

    descending_source = 6'b010100;
    index = 3;
    variable_index_read = descending_source[index];
  end

  final begin
    if (ascending_write !== 8'b00100000)
      $fatal(1, "ascending_write was %b, expected 00100000", ascending_write);
    if (ascending_read !== 1'b1)
      $fatal(1, "ascending_read was %b, expected 1", ascending_read);
    if (negative_bound_write !== 8'b01000000)
      $fatal(1, "negative_bound_write was %b, expected 01000000",
             negative_bound_write);
    if (ascending_part_write !== 8'b00011100)
      $fatal(1, "ascending_part_write was %b, expected 00011100",
             ascending_part_write);
    if (descending_offset_write !== 6'b000100)
      $fatal(1, "descending_offset_write was %b, expected 000100",
             descending_offset_write);
    if (ascending_plus_colon !== 8'b00000011)
      $fatal(1, "ascending_plus_colon was %b, expected 00000011",
             ascending_plus_colon);
    if (ascending_minus_colon !== 8'b00000100)
      $fatal(1, "ascending_minus_colon was %b, expected 00000100",
             ascending_minus_colon);
    if (variable_index_read !== 1'b1)
      $fatal(1, "variable_index_read was %b, expected 1", variable_index_read);
    $display("All checks passed");
  end
endmodule

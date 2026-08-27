// An assignment operator whose left-hand side is a bit-select or a
// part-select reads those bits, applies the operator, and stores the result
// back into the same bits, truncated to their width and leaving the rest of
// the vector alone. Any left-hand index expression is evaluated only once,
// so an index that has a side effect takes it once (LRM 11.4.1).
module Top;
  logic [7:0] set_bit;
  logic [7:0] clear_bit;
  logic [15:0] add_const_range;
  logic [15:0] or_const_range;
  logic [15:0] xor_indexed_up;
  logic [15:0] sub_indexed_down;
  logic [15:0] truncated_add;
  logic [7:0] once_target;
  int once_index;

  initial begin
    set_bit = 8'b0000_0000;
    set_bit[3] |= 1'b1;

    clear_bit = 8'b1111_1111;
    clear_bit[5] &= 1'b0;

    add_const_range = 16'h0050;
    add_const_range[7:4] += 4'd3;

    or_const_range = 16'h0030;
    or_const_range[7:4] |= 4'b1010;

    xor_indexed_up = 16'hAAAA;
    xor_indexed_up[4 +: 4] ^= 4'hF;

    sub_indexed_down = 16'h0F00;
    sub_indexed_down[11 -: 4] -= 4'd1;

    truncated_add = 16'h0010;
    truncated_add[7:0] += 16'h0123;

    once_target = 8'b0000_0000;
    once_index = 3;
    once_target[once_index++] |= 1'b1;
  end

  final begin
    if (set_bit !== 8'h08)
      $fatal(1, "set_bit[3] |= 1 gave %h, expected 08", set_bit);
    if (clear_bit !== 8'hDF)
      $fatal(1, "clear_bit[5] &= 0 gave %h, expected df", clear_bit);
    if (add_const_range !== 16'h0080)
      $fatal(1, "[7:4] += 3 gave %h, expected 0080", add_const_range);
    if (or_const_range !== 16'h00B0)
      $fatal(1, "[7:4] |= 1010 gave %h, expected 00b0", or_const_range);
    if (xor_indexed_up !== 16'hAA5A)
      $fatal(1, "[4 +: 4] ^= f gave %h, expected aa5a", xor_indexed_up);
    if (sub_indexed_down !== 16'h0E00)
      $fatal(1, "[11 -: 4] -= 1 gave %h, expected 0e00", sub_indexed_down);
    if (truncated_add !== 16'h0033)
      $fatal(1, "[7:0] += 0123 gave %h, expected 0033", truncated_add);
    if (once_target !== 8'h08)
      $fatal(1, "[once_index++] |= 1 gave %h, expected 08", once_target);
    if (once_index !== 4)
      $fatal(1, "index expression left once_index at %0d, expected 4",
             once_index);
    $display("All checks passed");
  end
endmodule

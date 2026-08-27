// The relational and equality operators yield 1'b1 when the relation holds
// and 1'b0 when it does not. They compare their operands as unsigned values
// when either operand is unsigned and as signed values when both are, so the
// same bit pattern orders differently under the two interpretations, and the
// comparison reaches every bit of the operands however wide they are
// (LRM 11.4.4, 11.4.5).
module Top;
  bit equal_true;
  bit equal_false;
  bit not_equal_true;
  bit not_equal_false;
  bit less_in_low_word;
  bit less_in_high_word;
  bit greater_in_high_word;
  bit less_across_word_boundary;
  bit greater_when_equal;
  bit less_equal_when_equal;
  bit greater_equal_when_equal;
  bit equal_when_less;
  bit not_equal_when_less;
  bit less_when_equal;
  bit less_when_greater;
  bit less_equal_when_less;
  bit less_equal_when_greater;
  bit greater_equal_when_less;
  bit greater_equal_when_greater;
  bit signed_negative_below_positive;
  bit signed_negative_above_negative;
  bit signed_negative_above_positive;
  bit unsigned_all_ones_above_one;
  bit signed_all_ones_above_one;

  initial begin
    equal_false = 1'b1;
    not_equal_false = 1'b1;
    greater_when_equal = 1'b1;
    signed_negative_above_positive = 1'b1;
    signed_all_ones_above_one = 1'b1;
    equal_when_less = 1'b1;
    less_when_equal = 1'b1;
    less_when_greater = 1'b1;
    less_equal_when_greater = 1'b1;
    greater_equal_when_less = 1'b1;

    begin
      bit [127:0] left;
      bit [127:0] right;
      bit signed [127:0] signed_left;
      bit signed [127:0] signed_right;

      left = 128'h00000000_00000001_FFFFFFFF_FFFFFFFF;
      right = 128'h00000000_00000001_FFFFFFFF_FFFFFFFF;
      equal_true = (left == right);
      not_equal_false = (left != right);
      greater_when_equal = (left > right);
      less_equal_when_equal = (left <= right);
      greater_equal_when_equal = (left >= right);

      right = 128'h00000000_00000001_FFFFFFFF_FFFFFFFE;
      equal_false = (left == right);
      not_equal_true = (left != right);
      less_in_low_word = (right < left);

      left = 128'h00000000_00000002_00000000_00000000;
      right = 128'h00000000_00000001_FFFFFFFF_FFFFFFFF;
      less_in_high_word = (right < left);
      greater_in_high_word = (left > right);

      left = 128'h00000000_00000000_FFFFFFFF_FFFFFFFF;
      right = 128'h00000000_00000001_00000000_00000000;
      less_across_word_boundary = (left < right);

      signed_left = -128'sd1;
      signed_right = 128'sd1;
      signed_negative_below_positive = (signed_left < signed_right);
      signed_negative_above_positive = (signed_left > signed_right);

      signed_left = -128'sd1;
      signed_right = -128'sd2;
      signed_negative_above_negative = (signed_left > signed_right);

      left = 128'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF;
      unsigned_all_ones_above_one = (left > 128'd1);

      signed_left = 128'shFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF;
      signed_all_ones_above_one = (signed_left > 128'sd1);

      // One wide pair read with a smaller, an equal and a larger left
      // operand, so no operator can pass by answering the same way whatever
      // it is given.
      left = 128'h00000000_00000001_00000000_00000000;
      right = 128'h00000000_00000002_00000000_00000000;
      equal_when_less = (left == right);
      not_equal_when_less = (left != right);
      less_equal_when_less = (left <= right);
      greater_equal_when_less = (left >= right);
      less_when_greater = (right < left);
      less_equal_when_greater = (right <= left);
      greater_equal_when_greater = (right >= left);
      right = left;
      less_when_equal = (left < right);
    end
  end

  final begin
    if (equal_true !== 1'b1)
      $fatal(1, "equal operands compared %b, expected 1", equal_true);
    if (equal_false !== 1'b0)
      $fatal(1, "unequal operands compared %b, expected 0", equal_false);
    if (not_equal_true !== 1'b1)
      $fatal(1, "!= on unequal operands was %b, expected 1", not_equal_true);
    if (not_equal_false !== 1'b0)
      $fatal(1, "!= on equal operands was %b, expected 0", not_equal_false);
    if (less_in_low_word !== 1'b1)
      $fatal(1, "a difference in the low word ordered %b, expected 1",
             less_in_low_word);
    if (less_in_high_word !== 1'b1)
      $fatal(1, "a difference in the high word ordered %b, expected 1",
             less_in_high_word);
    if (greater_in_high_word !== 1'b1)
      $fatal(1, "the reverse comparison was %b, expected 1",
             greater_in_high_word);
    if (less_across_word_boundary !== 1'b1)
      $fatal(1, "a comparison across the word boundary was %b, expected 1",
             less_across_word_boundary);
    if (greater_when_equal !== 1'b0)
      $fatal(1, "> on equal operands was %b, expected 0", greater_when_equal);
    if (less_equal_when_equal !== 1'b1)
      $fatal(1, "<= on equal operands was %b, expected 1",
             less_equal_when_equal);
    if (greater_equal_when_equal !== 1'b1)
      $fatal(1, ">= on equal operands was %b, expected 1",
             greater_equal_when_equal);
    if (equal_when_less !== 1'b0)
      $fatal(1, "== on a smaller left operand was %b, expected 0",
             equal_when_less);
    if (not_equal_when_less !== 1'b1)
      $fatal(1, "!= on a smaller left operand was %b, expected 1",
             not_equal_when_less);
    if (less_when_equal !== 1'b0)
      $fatal(1, "< on equal operands was %b, expected 0", less_when_equal);
    if (less_when_greater !== 1'b0)
      $fatal(1, "< on a larger left operand was %b, expected 0",
             less_when_greater);
    if (less_equal_when_less !== 1'b1)
      $fatal(1, "<= on a smaller left operand was %b, expected 1",
             less_equal_when_less);
    if (less_equal_when_greater !== 1'b0)
      $fatal(1, "<= on a larger left operand was %b, expected 0",
             less_equal_when_greater);
    if (greater_equal_when_less !== 1'b0)
      $fatal(1, ">= on a smaller left operand was %b, expected 0",
             greater_equal_when_less);
    if (greater_equal_when_greater !== 1'b1)
      $fatal(1, ">= on a larger left operand was %b, expected 1",
             greater_equal_when_greater);
    if (signed_negative_below_positive !== 1'b1)
      $fatal(1, "-1 < 1 signed was %b, expected 1",
             signed_negative_below_positive);
    if (signed_negative_above_positive !== 1'b0)
      $fatal(1, "-1 > 1 signed was %b, expected 0",
             signed_negative_above_positive);
    if (signed_negative_above_negative !== 1'b1)
      $fatal(1, "-1 > -2 signed was %b, expected 1",
             signed_negative_above_negative);
    if (unsigned_all_ones_above_one !== 1'b1)
      $fatal(1, "all ones > 1 unsigned was %b, expected 1",
             unsigned_all_ones_above_one);
    if (signed_all_ones_above_one !== 1'b0)
      $fatal(1, "all ones > 1 signed was %b, expected 0",
             signed_all_ones_above_one);
    $display("All checks passed");
  end
endmodule

// The bitwise operators combine their operands one bit position at a time
// and the reduction operators fold an operand into a single bit, both across
// the operand's whole width. A shift fills the vacated positions with zeros,
// except that an arithmetic right shift fills them with the sign bit when
// the result type is signed; a shift wider than the operand leaves nothing
// behind (LRM 11.4.8, 11.4.9, 11.4.10).
module Top;
  bit [127:0] bit_and;
  bit [127:0] bit_or;
  bit [127:0] bit_xor;
  bit [127:0] bit_not;
  bit [127:0] and_across_words;
  bit reduce_xor_one_bit;
  bit reduce_xor_even;
  bit reduce_or_upper_word;
  bit reduce_or_zero;
  bit reduce_and_all_ones;
  bit reduce_and_one_clear;
  bit reduce_nand_all_ones;
  bit [127:0] shl_small;
  bit [127:0] shl_across_words;
  bit [127:0] shl_large;
  bit [127:0] shl_past_width;
  bit [127:0] shr_small;
  bit [127:0] shr_across_words;
  bit [127:0] shr_then_shl;
  bit [127:0] ashr_unsigned_fills_zero;
  bit signed [127:0] ashr_signed_fills_sign;
  bit signed [127:0] ashr_signed_saturates;

  initial begin
    reduce_xor_even = 1'b1;
    reduce_or_zero = 1'b1;
    reduce_and_one_clear = 1'b1;
    reduce_nand_all_ones = 1'b1;
    shl_past_width = 128'd1;

    begin
      bit [127:0] left;
      bit [127:0] right;
      bit signed [127:0] signed_left;

      left = 128'd255;
      right = 128'd15;
      bit_and = left & right;
      bit_xor = left ^ right;

      left = 128'd240;
      right = 128'd15;
      bit_or = left | right;

      left = 128'd0;
      bit_not = ~left;

      left = 128'hFFFFFFFF_00000000_00000000_00000000;
      right = 128'h0F0F0F0F_0F0F0F0F_0F0F0F0F_0F0F0F0F;
      and_across_words = left & right;

      left = 128'h1;
      reduce_xor_one_bit = ^left;
      reduce_and_one_clear = &left;

      left = ~128'd0;
      reduce_xor_even = ^left;
      reduce_and_all_ones = &left;
      reduce_nand_all_ones = ~&left;

      left = 128'd1 << 64;
      reduce_or_upper_word = |left;

      left = 128'd0;
      reduce_or_zero = |left;

      left = 128'd1;
      shl_small = left << 4;
      shl_across_words = left << 64;
      shl_large = left << 100;
      shl_past_width = left << 200;

      left = 128'd256;
      shr_small = left >> 4;

      left = 128'h1_00000000_00000000;
      shr_across_words = left >> 64;

      left = 128'd1 << 64;
      shr_then_shl = left >> 32;

      left = 128'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF;
      ashr_unsigned_fills_zero = left >>> 1;

      signed_left = -128'sd64;
      ashr_signed_fills_sign = signed_left >>> 1;

      signed_left = -128'sd1;
      ashr_signed_saturates = signed_left >>> 100;
    end
  end

  final begin
    if (bit_and !== 128'hF)
      $fatal(1, "255 & 15 gave %h, expected f", bit_and);
    if (bit_or !== 128'hFF)
      $fatal(1, "240 | 15 gave %h, expected ff", bit_or);
    if (bit_xor !== 128'hF0)
      $fatal(1, "255 ^ 15 gave %h, expected f0", bit_xor);
    if (bit_not !== 128'hFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
      $fatal(1, "~0 gave %h, expected all ones", bit_not);
    if (and_across_words !== 128'h0F0F0F0F000000000000000000000000)
      $fatal(1, "an AND spanning the word boundary gave %h",
             and_across_words);
    if (reduce_xor_one_bit !== 1'b1)
      $fatal(1, "^(one set bit) was %b, expected 1", reduce_xor_one_bit);
    if (reduce_xor_even !== 1'b0)
      $fatal(1, "^(all ones) was %b, expected 0", reduce_xor_even);
    if (reduce_or_upper_word !== 1'b1)
      $fatal(1, "|(bit 64 set) was %b, expected 1", reduce_or_upper_word);
    if (reduce_or_zero !== 1'b0)
      $fatal(1, "|0 was %b, expected 0", reduce_or_zero);
    if (reduce_and_all_ones !== 1'b1)
      $fatal(1, "&(all ones) was %b, expected 1", reduce_and_all_ones);
    if (reduce_and_one_clear !== 1'b0)
      $fatal(1, "&(one set bit) was %b, expected 0", reduce_and_one_clear);
    if (reduce_nand_all_ones !== 1'b0)
      $fatal(1, "~&(all ones) was %b, expected 0", reduce_nand_all_ones);
    if (shl_small !== 128'h10)
      $fatal(1, "1 << 4 gave %h, expected 10", shl_small);
    if (shl_across_words !== 128'h10000000000000000)
      $fatal(1, "1 << 64 gave %h", shl_across_words);
    if (shl_large !== 128'h10000000000000000000000000)
      $fatal(1, "1 << 100 gave %h", shl_large);
    if (shl_past_width !== 128'd0)
      $fatal(1, "1 << 200 gave %h, expected 0", shl_past_width);
    if (shr_small !== 128'h10)
      $fatal(1, "256 >> 4 gave %h, expected 10", shr_small);
    if (shr_across_words !== 128'h1)
      $fatal(1, "a right shift across the word boundary gave %h",
             shr_across_words);
    if (shr_then_shl !== 128'h100000000)
      $fatal(1, "(1 << 64) >> 32 gave %h", shr_then_shl);
    if (ashr_unsigned_fills_zero !== 128'h7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
      $fatal(1, "an unsigned >>> gave %h", ashr_unsigned_fills_zero);
    if (ashr_signed_fills_sign !== -128'sd32)
      $fatal(1, "-64 >>> 1 gave %0d, expected -32", ashr_signed_fills_sign);
    if (ashr_signed_saturates !== -128'sd1)
      $fatal(1, "-1 >>> 100 gave %0d, expected -1", ashr_signed_saturates);
    $display("All checks passed");
  end
endmodule

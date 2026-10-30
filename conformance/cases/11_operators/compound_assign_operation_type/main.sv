// An assignment operator applies its operator as the blocking assignment
// `a = a op b` would, so the operation is carried out at the type the two
// operands fix between them and only its answer is brought to the target: at
// the wider operand's width, unsigned where either operand is, real where
// either is, and able to hold x where either can. A shift alone is sized by
// its left operand (LRM 11.4.1, 11.6.1 Table 11-21, 11.8.1, 11.8.2).
module Top;
  byte wider_add;
  byte wider_div;
  byte wider_mod;
  byte wider_signed_div;

  byte unsigned_div;
  byte unsigned_mod;
  int narrower_unsigned_div;
  int narrower_unsigned_mod;

  logic [7:0] partly_unknown;
  int unknown_add;
  int unknown_sub;
  int unknown_mul;
  int unknown_div;
  bit [7:0] unknown_xor;

  logic [15:0] unknown_above;
  logic [7:0] unknown_above_add;
  logic [7:0] unknown_above_div;
  logic [7:0] unknown_above_and;

  int real_mul;
  int real_div;
  int real_add;
  int real_sub;

  byte shift_right_logical;
  byte shift_right_arithmetic;

  logic signed [7:0] part_div;

  byte yielding_target;
  int yielded;

  initial begin
    wider_add = 100;
    wider_add += 16'h0101;
    wider_div = 100;
    wider_div /= 16'h0102;
    wider_mod = -100;
    wider_mod %= 16'd7;
    wider_signed_div = -100;
    wider_signed_div /= 16'sh0103;

    unsigned_div = -8;
    unsigned_div /= 8'd2;
    unsigned_mod = -8;
    unsigned_mod %= 8'd3;
    narrower_unsigned_div = -7;
    narrower_unsigned_div /= 4'd2;
    narrower_unsigned_mod = -7;
    narrower_unsigned_mod %= 4'd4;

    partly_unknown = 8'b0000_x001;
    unknown_add = 5;
    unknown_add += partly_unknown;
    unknown_sub = 5;
    unknown_sub -= partly_unknown;
    unknown_mul = 5;
    unknown_mul *= partly_unknown;
    unknown_div = 5;
    unknown_div /= partly_unknown;
    unknown_xor = 8'hff;
    unknown_xor ^= partly_unknown;

    unknown_above = 16'bxxxx_xxxx_0000_0011;
    unknown_above_add = 8'd10;
    unknown_above_add += unknown_above;
    unknown_above_div = 8'd10;
    unknown_above_div /= unknown_above;
    unknown_above_and = 8'd10;
    unknown_above_and &= unknown_above;

    real_mul = 3;
    real_mul *= 1.5;
    real_div = 7;
    real_div /= 2.0;
    real_add = -3;
    real_add += 1.5;
    real_sub = 5;
    real_sub -= 1.5;

    shift_right_logical = -128;
    shift_right_logical >>= 16'd1;
    shift_right_arithmetic = -128;
    shift_right_arithmetic >>>= 16'd1;

    part_div = 8'sb1111_1110;
    part_div[3:0] /= 8'h12;

    yielded = 77;
    yielding_target = 100;
    yielded = (yielding_target /= 16'h0102);
  end

  final begin
    if (wider_add !== 8'sd101)
      $fatal(1, "100 += 16'h0101 gave %0d, expected 101", wider_add);
    if (wider_div !== 8'sd0)
      $fatal(1, "100 /= 16'h0102 gave %0d, expected 0", wider_div);
    if (wider_mod !== 8'sd2)
      $fatal(1, "-100 %%= 16'd7 gave %0d, expected 2", wider_mod);
    if (wider_signed_div !== 8'sd0)
      $fatal(1, "-100 /= 16'sh0103 gave %0d, expected 0", wider_signed_div);

    if (unsigned_div !== 8'sd124)
      $fatal(1, "-8 /= 8'd2 gave %0d, expected 124", unsigned_div);
    if (unsigned_mod !== 8'sd2)
      $fatal(1, "-8 %%= 8'd3 gave %0d, expected 2", unsigned_mod);
    if (narrower_unsigned_div !== 2147483644)
      $fatal(1, "-7 /= 4'd2 gave %0d, expected 2147483644",
             narrower_unsigned_div);
    if (narrower_unsigned_mod !== 1)
      $fatal(1, "-7 %%= 4'd4 gave %0d, expected 1", narrower_unsigned_mod);

    if (unknown_add !== 0)
      $fatal(1, "5 += a partly unknown value gave %0d, expected 0",
             unknown_add);
    if (unknown_sub !== 0)
      $fatal(1, "5 -= a partly unknown value gave %0d, expected 0",
             unknown_sub);
    if (unknown_mul !== 0)
      $fatal(1, "5 *= a partly unknown value gave %0d, expected 0",
             unknown_mul);
    if (unknown_div !== 0)
      $fatal(1, "5 /= a partly unknown value gave %0d, expected 0",
             unknown_div);
    if (unknown_xor !== 8'hf6)
      $fatal(1, "ff ^= 0000x001 gave %h, expected f6", unknown_xor);

    if (unknown_above_add !== 8'bxxxx_xxxx)
      $fatal(1, "10 += a value unknown above the target gave %b, expected x",
             unknown_above_add);
    if (unknown_above_div !== 8'bxxxx_xxxx)
      $fatal(1, "10 /= a value unknown above the target gave %b, expected x",
             unknown_above_div);
    if (unknown_above_and !== 8'b0000_0010)
      $fatal(1, "10 &= a value unknown above the target gave %b, expected 2",
             unknown_above_and);

    if (real_mul !== 5) $fatal(1, "3 *= 1.5 gave %0d, expected 5", real_mul);
    if (real_div !== 4) $fatal(1, "7 /= 2.0 gave %0d, expected 4", real_div);
    if (real_add !== -2)
      $fatal(1, "-3 += 1.5 gave %0d, expected -2", real_add);
    if (real_sub !== 4) $fatal(1, "5 -= 1.5 gave %0d, expected 4", real_sub);

    if (shift_right_logical !== 8'sd64)
      $fatal(1, "-128 >>= 16'd1 gave %0d, expected 64", shift_right_logical);
    if (shift_right_arithmetic !== -8'sd64)
      $fatal(1, "-128 >>>= 16'd1 gave %0d, expected -64",
             shift_right_arithmetic);

    if (part_div !== 8'sb1111_0000)
      $fatal(1, "a nibble holding 14 /= 8'h12 left %b, expected 11110000",
             part_div);

    if (yielding_target !== 8'sd0)
      $fatal(1, "100 /= 16'h0102 read as an expression left %0d, expected 0",
             yielding_target);
    if (yielded !== 0)
      $fatal(1, "100 /= 16'h0102 read as an expression gave %0d, expected 0",
             yielded);
    $display("All checks passed");
  end
endmodule

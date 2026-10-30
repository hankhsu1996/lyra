// An assignment operator is semantically equivalent to the blocking
// assignment that applies the operator to the current value of the left-hand
// side and the right-hand side, so the operation is sized and signed by the
// ordinary expression rules -- a narrower unsigned operand is zero-extended
// rather than the target being narrowed -- and the result is then assigned
// back (LRM 11.4.1, 11.6.1, 11.8.1).
module Top;
  int add_result;
  int sub_result;
  int mul_result;
  int div_result;
  int mod_result;
  int and_result;
  int or_result;
  int xor_result;
  logic [15:0] shift_left_logical;
  logic [15:0] shift_right_logical;
  int shift_left_arith;
  int shift_right_arith;
  int shift_right_fills_zero;
  int narrow_and_operand;
  int narrow_or_operand;

  initial begin
    add_result = 10;
    add_result += 3;

    sub_result = 10;
    sub_result -= 3;

    mul_result = 10;
    mul_result *= 3;

    div_result = 10;
    div_result /= 3;

    mod_result = 10;
    mod_result %= 3;

    and_result = 32'hF0F0;
    and_result &= 32'hFF00;

    or_result = 32'h0F0F;
    or_result |= 32'hF000;

    xor_result = 32'hAAAA;
    xor_result ^= 32'hFFFF;

    shift_left_logical = 16'h0001;
    shift_left_logical <<= 4;

    shift_right_logical = 16'hF000;
    shift_right_logical >>= 4;

    shift_left_arith = 32'sd1;
    shift_left_arith <<<= 4;

    shift_right_arith = -32'sd16;
    shift_right_arith >>>= 2;

    shift_right_fills_zero = -32'sd16;
    shift_right_fills_zero >>= 2;

    narrow_and_operand = 32'hFFFF_FF0F;
    begin
      logic [3:0] nibble;
      nibble = 4'b1010;
      narrow_and_operand &= nibble;
    end

    narrow_or_operand = 32'h0000_0000;
    begin
      logic [7:0] byte_operand;
      byte_operand = 8'b0000_0011;
      narrow_or_operand |= byte_operand;
    end
  end

  final begin
    if (add_result !== 13)
      $fatal(1, "10 += 3 gave %0d, expected 13", add_result);
    if (sub_result !== 7)
      $fatal(1, "10 -= 3 gave %0d, expected 7", sub_result);
    if (mul_result !== 30)
      $fatal(1, "10 *= 3 gave %0d, expected 30", mul_result);
    if (div_result !== 3)
      $fatal(1, "10 /= 3 gave %0d, expected 3", div_result);
    if (mod_result !== 1)
      $fatal(1, "10 %%= 3 gave %0d, expected 1", mod_result);
    if (and_result !== 32'h0000F000)
      $fatal(1, "f0f0 &= ff00 gave %h, expected 0000f000", and_result);
    if (or_result !== 32'h0000FF0F)
      $fatal(1, "0f0f |= f000 gave %h, expected 0000ff0f", or_result);
    if (xor_result !== 32'h00005555)
      $fatal(1, "aaaa ^= ffff gave %h, expected 00005555", xor_result);
    if (shift_left_logical !== 16'h0010)
      $fatal(1, "0001 <<= 4 gave %h, expected 0010", shift_left_logical);
    if (shift_right_logical !== 16'h0F00)
      $fatal(1, "f000 >>= 4 gave %h, expected 0f00", shift_right_logical);
    if (shift_left_arith !== 16)
      $fatal(1, "1 <<<= 4 gave %0d, expected 16", shift_left_arith);
    if (shift_right_arith !== -4)
      $fatal(1, "-16 >>>= 2 gave %0d, expected -4", shift_right_arith);
    if (shift_right_fills_zero !== 32'h3FFFFFFC)
      $fatal(1, "-16 >>= 2 gave %h, expected 3ffffffc",
             shift_right_fills_zero);
    if (narrow_and_operand !== 32'h0000000A)
      $fatal(1, "ffffff0f &= 4'b1010 gave %h, expected 0000000a",
             narrow_and_operand);
    if (narrow_or_operand !== 32'h00000003)
      $fatal(1, "0 |= 8'b00000011 gave %h, expected 00000003",
             narrow_or_operand);
    $display("All checks passed");
  end
endmodule

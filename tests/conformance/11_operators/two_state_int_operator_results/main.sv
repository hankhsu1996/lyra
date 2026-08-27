// An operator applied to 2-state operands gives the result it would give if
// every value were 4-state, and that result is then brought back into the
// 2-state value set, so an operation that would be x -- division or modulus
// by zero, or arithmetic reading an unknown operand -- lands as 0 in a
// 2-state destination while a 4-state destination keeps the x. The int type
// is a signed 32-bit 2-state type, so its division truncates towards zero,
// its modulus takes the sign of the dividend, its negation and bitwise
// negation run in 32 bits, and a reduction spans all 32 of them
// (LRM 11.3.4, 11.4.3, 11.4.3.1, 11.4.9, 11.8.1).
module Top;
  int sum;
  int difference;
  int negative_difference;
  int product;
  int negative_product;
  int quotient;
  int negative_quotient;
  int remainder;
  int negative_remainder;
  int divide_by_zero_two_state;
  integer divide_by_zero_four_state;
  int modulo_by_zero_two_state;
  integer modulo_by_zero_four_state;
  int unknown_operand_sum;
  integer unknown_operand_sum_four_state;
  int unknown_bits_or;
  int bitwise_and;
  int bitwise_or;
  int bitwise_xor;
  int bitwise_xnor;
  int bitwise_not;
  int unary_minus;
  bit logical_not_nonzero;
  bit logical_not_zero;
  bit logical_and;
  bit logical_or;
  bit reduction_and_all_ones;
  bit reduction_and_one_zero;
  bit reduction_or_zero;
  bit reduction_xor_odd;
  bit reduction_xor_even;
  bit reduction_nor_zero;
  bit signed_less;
  bit signed_greater;

  initial begin
    int a;
    int b;
    int zero;

    divide_by_zero_two_state = 1;
    modulo_by_zero_two_state = 1;
    unknown_operand_sum = 1;
    divide_by_zero_four_state = 0;
    modulo_by_zero_four_state = 0;
    unknown_operand_sum_four_state = 0;
    logical_not_nonzero = 1'b1;
    logical_and = 1'b1;
    reduction_and_one_zero = 1'b1;
    reduction_or_zero = 1'b1;
    reduction_xor_even = 1'b1;
    signed_greater = 1'b1;

    a = 30;
    b = 12;
    sum = a + b;
    difference = a - b;
    negative_difference = b - a;

    b = 7;
    product = 5 * b;
    a = -5;
    negative_product = a * b;

    // A dividend that does not divide exactly, so truncation towards zero is
    // told apart from rounding away from it.
    a = 103;
    b = 4;
    quotient = a / b;
    a = -103;
    negative_quotient = a / b;

    a = 17;
    b = 5;
    remainder = a % b;
    a = -10;
    b = 3;
    negative_remainder = a % b;

    zero = 0;
    a = 17;
    divide_by_zero_two_state = a / zero;
    divide_by_zero_four_state = a / zero;
    modulo_by_zero_two_state = a % zero;
    modulo_by_zero_four_state = a % zero;

    unknown_operand_sum = 'x + 8;
    unknown_operand_sum_four_state = 'x + 8;

    // The unknown bits reach only the positions they occupy, and the known
    // ones survive the coercion.
    unknown_bits_or = 'b01xz | 8;

    a = 5;
    b = 3;
    bitwise_and = a & b;
    bitwise_or = a | b;
    bitwise_xor = a ^ b;
    bitwise_xnor = a ~^ b;
    bitwise_not = ~a;
    unary_minus = -a;
    logical_not_nonzero = !a;
    b = 0;
    logical_not_zero = !b;
    logical_and = a && b;
    logical_or = a || b;

    a = -1;
    reduction_and_all_ones = &a;
    reduction_xor_even = ^a;
    a = 32'hFFFF_FFFE;
    reduction_and_one_zero = &a;
    a = 0;
    reduction_or_zero = |a;
    reduction_nor_zero = ~|a;
    a = 1;
    reduction_xor_odd = ^a;

    a = -1;
    b = 100;
    signed_less = (a < b);
    signed_greater = (a > b);
  end

  final begin
    if (sum !== 42) $fatal(1, "sum was %0d, expected 42", sum);
    if (difference !== 18)
      $fatal(1, "difference was %0d, expected 18", difference);
    if (negative_difference !== -18)
      $fatal(1, "negative_difference was %0d, expected -18",
             negative_difference);
    if (product !== 35) $fatal(1, "product was %0d, expected 35", product);
    if (negative_product !== -35)
      $fatal(1, "negative_product was %0d, expected -35", negative_product);
    if (quotient !== 25) $fatal(1, "quotient was %0d, expected 25", quotient);
    if (negative_quotient !== -25)
      $fatal(1, "negative_quotient was %0d, expected -25", negative_quotient);
    if (remainder !== 2)
      $fatal(1, "remainder was %0d, expected 2", remainder);
    if (negative_remainder !== -1)
      $fatal(1, "negative_remainder was %0d, expected -1", negative_remainder);
    if (divide_by_zero_two_state !== 0)
      $fatal(1, "divide_by_zero_two_state was %0d, expected 0",
             divide_by_zero_two_state);
    if (divide_by_zero_four_state !== 32'bx)
      $fatal(1, "divide_by_zero_four_state was %b, expected all x",
             divide_by_zero_four_state);
    if (modulo_by_zero_two_state !== 0)
      $fatal(1, "modulo_by_zero_two_state was %0d, expected 0",
             modulo_by_zero_two_state);
    if (modulo_by_zero_four_state !== 32'bx)
      $fatal(1, "modulo_by_zero_four_state was %b, expected all x",
             modulo_by_zero_four_state);
    if (unknown_operand_sum !== 0)
      $fatal(1, "unknown_operand_sum was %0d, expected 0",
             unknown_operand_sum);
    if (unknown_operand_sum_four_state !== 32'bx)
      $fatal(1, "unknown_operand_sum_four_state was %b, expected all x",
             unknown_operand_sum_four_state);
    if (unknown_bits_or !== 12)
      $fatal(1, "unknown_bits_or was %0d, expected 12", unknown_bits_or);
    if (bitwise_and !== 1)
      $fatal(1, "bitwise_and was %0d, expected 1", bitwise_and);
    if (bitwise_or !== 7)
      $fatal(1, "bitwise_or was %0d, expected 7", bitwise_or);
    if (bitwise_xor !== 6)
      $fatal(1, "bitwise_xor was %0d, expected 6", bitwise_xor);
    if (bitwise_xnor !== -7)
      $fatal(1, "bitwise_xnor was %0d, expected -7", bitwise_xnor);
    if (bitwise_not !== -6)
      $fatal(1, "bitwise_not was %0d, expected -6", bitwise_not);
    if (unary_minus !== -5)
      $fatal(1, "unary_minus was %0d, expected -5", unary_minus);
    if (logical_not_nonzero !== 1'b0)
      $fatal(1, "logical_not_nonzero was %b, expected 0", logical_not_nonzero);
    if (logical_not_zero !== 1'b1)
      $fatal(1, "logical_not_zero was %b, expected 1", logical_not_zero);
    if (logical_and !== 1'b0)
      $fatal(1, "logical_and was %b, expected 0", logical_and);
    if (logical_or !== 1'b1)
      $fatal(1, "logical_or was %b, expected 1", logical_or);
    if (reduction_and_all_ones !== 1'b1)
      $fatal(1, "reduction_and_all_ones was %b, expected 1",
             reduction_and_all_ones);
    if (reduction_and_one_zero !== 1'b0)
      $fatal(1, "reduction_and_one_zero was %b, expected 0",
             reduction_and_one_zero);
    if (reduction_or_zero !== 1'b0)
      $fatal(1, "reduction_or_zero was %b, expected 0", reduction_or_zero);
    if (reduction_nor_zero !== 1'b1)
      $fatal(1, "reduction_nor_zero was %b, expected 1", reduction_nor_zero);
    if (reduction_xor_odd !== 1'b1)
      $fatal(1, "reduction_xor_odd was %b, expected 1", reduction_xor_odd);
    if (reduction_xor_even !== 1'b0)
      $fatal(1, "reduction_xor_even was %b, expected 0", reduction_xor_even);
    if (signed_less !== 1'b1)
      $fatal(1, "signed_less was %b, expected 1", signed_less);
    if (signed_greater !== 1'b0)
      $fatal(1, "signed_greater was %b, expected 0", signed_greater);
    $display("All checks passed");
  end
endmodule

// A continuous assignment drives the value of its right-hand side onto its
// left-hand side, and whenever any operand in that expression changes value the
// whole right-hand side is evaluated again and the new value driven, so the
// target follows its operands instead of holding what it was given once
// (LRM 10.3.2). Every operand the expression reads belongs to that set,
// including one reached through an array element select and one read from the
// target of another continuous assignment, which propagates a change along a
// chain. A right-hand side that reads no operand at all still drives its value.
module Top;
  int operand;
  int taken, rejected;
  bit selector;
  int elements [2];

  int incremented;
  int chain_middle;
  int chain_end;
  int chosen;
  int element_sum;
  int constant_target;

  assign incremented = operand + 1;
  assign chain_middle = operand;
  assign chain_end = chain_middle + 1;
  assign chosen = selector ? taken : rejected;
  assign element_sum = elements[0] + elements[1];
  assign constant_target = 42;

  int incremented_first;
  int chain_end_first;
  int chosen_first;
  int element_sum_first;

  initial begin
    operand = 3;
    selector = 1'b1;
    taken = 7;
    rejected = 9;
    elements[0] = 3;
    elements[1] = 4;
    #1;
    incremented_first = incremented;
    chain_end_first = chain_end;
    chosen_first = chosen;
    element_sum_first = element_sum;

    operand = 10;
    selector = 1'b0;
    elements[0] = 20;
    #1;
  end

  final begin
    if (incremented_first !== 4)
      $fatal(1, "incremented_first was %0d, expected 4", incremented_first);
    if (chain_end_first !== 4)
      $fatal(1, "chain_end_first was %0d, expected 4", chain_end_first);
    if (chosen_first !== 7)
      $fatal(1, "chosen_first was %0d, expected 7", chosen_first);
    if (element_sum_first !== 7)
      $fatal(1, "element_sum_first was %0d, expected 7", element_sum_first);

    if (incremented !== 11)
      $fatal(1, "incremented was %0d, expected 11", incremented);
    if (chain_end !== 11)
      $fatal(1, "chain_end was %0d, expected 11", chain_end);
    if (chosen !== 9) $fatal(1, "chosen was %0d, expected 9", chosen);
    if (element_sum !== 24)
      $fatal(1, "element_sum was %0d, expected 24", element_sum);
    if (constant_target !== 42)
      $fatal(1, "constant_target was %0d, expected 42", constant_target);
    $display("All checks passed");
  end
endmodule

// The operators &&, ||, -> and ?: use short-circuit evaluation: an operand
// whose value is not needed to settle the result is not evaluated, and none of
// the side effects its evaluation would have had occur (LRM 11.3.5). Every
// other operator evaluates all of its operands. So a function called in the
// operand that is not needed does not run, and the same function called where
// its value is needed runs once.
//
// That covers everything evaluating the operand takes, not only what it
// calls: a write to a property reaches the object through a handle the
// operand computes (LRM 8.4), and that handle is not computed in an operand
// that is not needed. The same holds in an arm of a conditional inside a
// `with` clause, which is evaluated once per item (LRM 7.12).
module Top;
  class Holder;
    int count;
  endclass

  int calls;
  Holder kept;
  int items [3];

  function automatic bit counted();
    calls = calls + 1;
    return 1'b1;
  endfunction

  function automatic int counted_value();
    calls = calls + 1;
    return 7;
  endfunction

  function automatic Holder counted_holder();
    calls = calls + 1;
    return kept;
  endfunction

  bit low;
  bit high;
  bit taken;
  int chosen;
  int written;
  int summed;

  int conditional_write_skipped = -1;
  int conditional_write_needed = -1;
  int and_write_skipped = -1;
  int or_write_skipped = -1;
  int implication_write_skipped = -1;
  int with_write_skipped = -1;

  int and_skipped = -1;
  int and_needed = -1;
  int or_skipped = -1;
  int or_needed = -1;
  int implication_skipped = -1;
  int implication_needed = -1;
  int conditional_skipped = -1;
  int bitwise_and = -1;

  initial begin
    low = 1'b0;
    high = 1'b1;

    calls = 0;
    taken = low && counted();
    and_skipped = calls;
    calls = 0;
    taken = high && counted();
    and_needed = calls;

    calls = 0;
    taken = high || counted();
    or_skipped = calls;
    calls = 0;
    taken = low || counted();
    or_needed = calls;

    calls = 0;
    taken = low -> counted();
    implication_skipped = calls;
    calls = 0;
    taken = high -> counted();
    implication_needed = calls;

    calls = 0;
    chosen = low ? counted_value() : 3;
    conditional_skipped = calls;

    calls = 0;
    taken = low & counted();
    bitwise_and = calls;

    kept = new;
    calls = 0;
    written = low ? counted_holder().count++ : 3;
    conditional_write_skipped = calls;
    calls = 0;
    written = high ? (counted_holder().count += 5) : 3;
    conditional_write_needed = calls;

    calls = 0;
    taken = low && ((counted_holder().count = 9) > 0);
    and_write_skipped = calls;
    calls = 0;
    taken = high || ((counted_holder().count = 9) > 0);
    or_write_skipped = calls;
    calls = 0;
    taken = low -> ((counted_holder().count = 9) > 0);
    implication_write_skipped = calls;

    items = '{1, 2, 3};
    calls = 0;
    summed = items.sum() with (low ? counted_holder().count++ : item);
    with_write_skipped = calls;
  end

  final begin
    if (and_skipped !== 0)
      $fatal(1, "the right operand of && ran %0d times behind a 0, expected 0",
             and_skipped);
    if (and_needed !== 1)
      $fatal(1, "the right operand of && ran %0d times behind a 1, expected 1",
             and_needed);
    if (or_skipped !== 0)
      $fatal(1, "the right operand of || ran %0d times behind a 1, expected 0",
             or_skipped);
    if (or_needed !== 1)
      $fatal(1, "the right operand of || ran %0d times behind a 0, expected 1",
             or_needed);
    if (implication_skipped !== 0)
      $fatal(1, "the right operand of -> ran %0d times behind a 0, expected 0",
             implication_skipped);
    if (implication_needed !== 1)
      $fatal(1, "the right operand of -> ran %0d times behind a 1, expected 1",
             implication_needed);
    if (conditional_skipped !== 0)
      $fatal(1, "the arm ?: did not take ran %0d times, expected 0",
             conditional_skipped);
    if (chosen !== 3)
      $fatal(1, "?: chose %0d, expected 3", chosen);
    if (bitwise_and !== 1)
      $fatal(1, "the right operand of & ran %0d times, expected 1",
             bitwise_and);
    if (conditional_write_skipped !== 0)
      $fatal(1, "a property write in the arm ?: did not take reached its handle %0d times, expected 0",
             conditional_write_skipped);
    if (conditional_write_needed !== 1)
      $fatal(1, "a property write in the arm ?: took reached its handle %0d times, expected 1",
             conditional_write_needed);
    if (and_write_skipped !== 0)
      $fatal(1, "a property write behind && reached its handle %0d times, expected 0",
             and_write_skipped);
    if (or_write_skipped !== 0)
      $fatal(1, "a property write behind || reached its handle %0d times, expected 0",
             or_write_skipped);
    if (implication_write_skipped !== 0)
      $fatal(1, "a property write behind -> reached its handle %0d times, expected 0",
             implication_write_skipped);
    if (kept.count !== 5)
      $fatal(1, "the property was left at %0d, expected 5", kept.count);
    if (with_write_skipped !== 0)
      $fatal(1, "a property write in an arm a with clause did not take reached its handle %0d times, expected 0",
             with_write_skipped);
    if (summed !== 6)
      $fatal(1, "the with clause summed to %0d, expected 6", summed);
    $display("All checks passed");
  end
endmodule

// What decides whether the second operand of && || -> is evaluated is the
// first operand's truth, whatever kind of value it is: the first operand is
// always evaluated, && and -> skip the second when the first is logically
// false, || skips it when the first is logically true, and an unknown first
// operand decides nothing, so the second runs and the result follows the
// operator's truth table (LRM 11.4.7, 11.3.5). A real operand is true when it is
// nonzero (LRM 11.3.1), and a handle when it is not null. Whatever a skipped
// operand would have done does not happen, including a write and a run-time
// error, wherever the operator stands. <-> evaluates each operand exactly once.
module Top;
  int calls;

  function automatic logic counted(logic v);
    calls = calls + 1;
    return v;
  endfunction

  function automatic real counted_real(real v);
    calls = calls + 1;
    return v;
  endfunction

  function automatic logic same(logic v);
    return v;
  endfunction

  class Node;
    bit ready;
  endclass

  logic unknown;
  real zero;
  real half;
  int whole;
  Node none;
  Node some;

  int unknown_and_false_calls = -1;
  logic unknown_and_false = 1'b1;
  int unknown_and_true_calls = -1;
  logic unknown_and_true = 1'b1;
  int unknown_or_true_calls = -1;
  logic unknown_or_true = 1'b0;
  int unknown_or_false_calls = -1;
  logic unknown_or_false = 1'b1;
  int unknown_implies_true_calls = -1;
  logic unknown_implies_true = 1'b0;
  int unknown_implies_false_calls = -1;
  logic unknown_implies_false = 1'b1;

  int real_and_skipped = -1;
  int real_or_skipped = -1;
  int real_implies_skipped = -1;
  logic real_and_unknown = 1'b0;
  logic unknown_and_real = 1'b0;
  logic real_or_unknown = 1'b0;
  logic real_implies_unknown = 1'b0;
  logic real_equivalent_unknown = 1'b0;
  logic whole_and_unknown = 1'b0;
  logic unknown_or_whole = 1'b0;

  int equivalence_calls = -1;
  logic equivalence = 1'b0;

  int null_guard = -1;
  int null_guard_through_or = -1;
  int increment_skipped = -1;
  int nested_skipped = -1;
  int nested_needed = -1;
  int argument_skipped = -1;
  int condition_skipped = -1;
  int loop_condition_calls = -1;

  initial begin
    logic result;
    int i;
    int passes;

    unknown = 1'bx;
    zero = 0.0;
    half = 0.5;
    whole = 3;
    none = null;
    some = new;

    calls = 0;
    unknown_and_false = unknown && counted(1'b0);
    unknown_and_false_calls = calls;
    calls = 0;
    unknown_and_true = unknown && counted(1'b1);
    unknown_and_true_calls = calls;
    calls = 0;
    unknown_or_true = unknown || counted(1'b1);
    unknown_or_true_calls = calls;
    calls = 0;
    unknown_or_false = unknown || counted(1'b0);
    unknown_or_false_calls = calls;
    calls = 0;
    unknown_implies_true = unknown -> counted(1'b1);
    unknown_implies_true_calls = calls;
    calls = 0;
    unknown_implies_false = unknown -> counted(1'b0);
    unknown_implies_false_calls = calls;

    calls = 0;
    result = zero && counted_real(1.0);
    real_and_skipped = calls;
    calls = 0;
    result = half || counted_real(1.0);
    real_or_skipped = calls;
    calls = 0;
    result = zero -> counted_real(0.0);
    real_implies_skipped = calls;

    real_and_unknown = half && unknown;
    unknown_and_real = unknown && half;
    real_or_unknown = zero || unknown;
    real_implies_unknown = half -> unknown;
    real_equivalent_unknown = half <-> unknown;
    whole_and_unknown = whole && unknown;
    unknown_or_whole = unknown || whole;

    calls = 0;
    equivalence = counted(1'b0) <-> counted(1'b0);
    equivalence_calls = calls;

    null_guard = 0;
    if (none != null && none.ready) null_guard = 2;
    else null_guard = 1;
    null_guard_through_or = 0;
    if (none == null || none.ready) null_guard_through_or = 1;
    else null_guard_through_or = 2;

    i = 0;
    result = 1'b0 && (i++ > 0);
    increment_skipped = i;

    calls = 0;
    result = 1'b0 && (1'b1 || counted(1'b1));
    nested_skipped = calls;
    calls = 0;
    result = (1'b0 && counted(1'b1)) || counted(1'b1);
    nested_needed = calls;

    calls = 0;
    result = same(1'b0 && counted(1'b1));
    argument_skipped = calls;

    calls = 0;
    if (1'b0 && counted(1'b1)) calls = calls + 100;
    condition_skipped = calls;

    calls = 0;
    passes = 0;
    while (passes < 3 && counted(1'b1)) passes = passes + 1;
    loop_condition_calls = calls;
  end

  final begin
    if (unknown_and_false_calls !== 1 || unknown_and_false !== 1'b0)
      $fatal(1, "x && 0 ran its operand %0d times and was %b, expected 1 and 0",
             unknown_and_false_calls, unknown_and_false);
    if (unknown_and_true_calls !== 1 || unknown_and_true !== 1'bx)
      $fatal(1, "x && 1 ran its operand %0d times and was %b, expected 1 and x",
             unknown_and_true_calls, unknown_and_true);
    if (unknown_or_true_calls !== 1 || unknown_or_true !== 1'b1)
      $fatal(1, "x || 1 ran its operand %0d times and was %b, expected 1 and 1",
             unknown_or_true_calls, unknown_or_true);
    if (unknown_or_false_calls !== 1 || unknown_or_false !== 1'bx)
      $fatal(1, "x || 0 ran its operand %0d times and was %b, expected 1 and x",
             unknown_or_false_calls, unknown_or_false);
    if (unknown_implies_true_calls !== 1 || unknown_implies_true !== 1'b1)
      $fatal(1, "x -> 1 ran its operand %0d times and was %b, expected 1 and 1",
             unknown_implies_true_calls, unknown_implies_true);
    if (unknown_implies_false_calls !== 1 || unknown_implies_false !== 1'bx)
      $fatal(1, "x -> 0 ran its operand %0d times and was %b, expected 1 and x",
             unknown_implies_false_calls, unknown_implies_false);

    if (real_and_skipped !== 0)
      $fatal(1, "0.0 && f ran f %0d times, expected 0", real_and_skipped);
    if (real_or_skipped !== 0)
      $fatal(1, "0.5 || f ran f %0d times, expected 0", real_or_skipped);
    if (real_implies_skipped !== 0)
      $fatal(1, "0.0 -> f ran f %0d times, expected 0", real_implies_skipped);
    if (real_and_unknown !== 1'bx)
      $fatal(1, "0.5 && x was %b, expected x", real_and_unknown);
    if (unknown_and_real !== 1'bx)
      $fatal(1, "x && 0.5 was %b, expected x", unknown_and_real);
    if (real_or_unknown !== 1'bx)
      $fatal(1, "0.0 || x was %b, expected x", real_or_unknown);
    if (real_implies_unknown !== 1'bx)
      $fatal(1, "0.5 -> x was %b, expected x", real_implies_unknown);
    if (real_equivalent_unknown !== 1'bx)
      $fatal(1, "0.5 <-> x was %b, expected x", real_equivalent_unknown);
    if (whole_and_unknown !== 1'bx)
      $fatal(1, "3 && x was %b, expected x", whole_and_unknown);
    if (unknown_or_whole !== 1'b1)
      $fatal(1, "x || 3 was %b, expected 1", unknown_or_whole);

    if (equivalence_calls !== 2 || equivalence !== 1'b1)
      $fatal(1, "f <-> f ran %0d calls and was %b, expected 2 and 1",
             equivalence_calls, equivalence);

    if (null_guard !== 1)
      $fatal(1, "a null guard with && took arm %0d, expected 1", null_guard);
    if (null_guard_through_or !== 1)
      $fatal(1, "a null guard with || took arm %0d, expected 1",
             null_guard_through_or);
    if (increment_skipped !== 0)
      $fatal(1, "a skipped increment left i at %0d, expected 0",
             increment_skipped);
    if (nested_skipped !== 0)
      $fatal(1, "0 && (1 || f) ran f %0d times, expected 0", nested_skipped);
    if (nested_needed !== 1)
      $fatal(1, "(0 && f) || f ran f %0d times, expected 1", nested_needed);
    if (argument_skipped !== 0)
      $fatal(1, "an argument 0 && f ran f %0d times, expected 0",
             argument_skipped);
    if (condition_skipped !== 0)
      $fatal(1, "a condition 0 && f ran f %0d times, expected 0",
             condition_skipped);
    if (loop_condition_calls !== 3)
      $fatal(1, "a loop condition n < 3 && f ran f %0d times, expected 3",
             loop_condition_calls);
    $display("All checks passed");
  end
endmodule

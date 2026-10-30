// A sampled value function that writes no clocking event counts ticks of the
// clock in force where it is called (LRM 16.9.3): inside an assertion, the
// assertion's clock by clock flow, whether the clock is written on the
// statement, declared with the property, or taken from the clocking block the
// property is declared in (LRM 16.16); in an action block, the assertion's
// leading clock; and outside any assertion or procedure, the default clocking.
// `$inferred_clock` names the clock in force where its property is
// instantiated (LRM 16.14.7).
//
// The default clocking here falls on the other edge, so a read counting its
// ticks answers differently from one counting the assertion's.
module Top;
  logic clk = 0;
  int n = 0;
  logic never_rises = 0;

  default clocking falls @(negedge clk);
  endclocking

  clocking rises @(posedge clk);
    property steps_in_block;
      $past(n) == (n == 0 ? 0 : n - 1);
    endproperty
  endclocking

  property steps_declared;
    @(posedge clk) $past(n) == (n == 0 ? 0 : n - 1);
  endproperty

  property steps_inferred(x, c = $inferred_clock);
    @c $past(x) == (x == 0 ? 0 : x - 1);
  endproperty

  // `c` is the clock where the instance stands, not the clock written before
  // the read: at the k-th rise counted from zero, at 10k + 5, the latest fall
  // is at 10k, where `x` was already k, and before the first fall it reads its
  // default, 0.
  property past_at_the_instance(x, c = $inferred_clock);
    @(posedge clk) $past(x, 1, 1, @c) == x;
  endproperty

  // A list holding `$inferred_clock` stands for the list with that clock in
  // its place, here a rise of `clk` or of a signal that never rises.
  property steps_listed(x, c = $inferred_clock);
    @(c or posedge never_rises) $past(x) == (x == 0 ? 0 : x - 1);
  endproperty

  // A function body is no procedure, so its `$past` counts ticks of the
  // default clocking.
  function automatic int last_falling();
    return $past(n);
  endfunction

  int written_fails = 0;
  int declared_fails = 0;
  int block_fails = 0;
  int inferred_written_fails = 0;
  int inferred_default_passes = 0;
  int inferred_default_fails = 0;
  int action_checked = 0;
  int action_wrong = 0;
  int instance_clock_fails = 0;
  int listed_fails = 0;
  int listed_checked = 0;
  int listed_action_wrong = 0;
  int juxtaposed_fails = 0;
  int from_function = -1;

  // A continuous assignment is no procedure, so its `$past` counts ticks of
  // the default clocking.
  wire logic [31:0] last_fall = $past(n);
  // At 97 the latest fall is at 90, where `n` was 9, and `n` last moved at
  // 95, so no fall lies between that change and the read.
  int fall_read = -1;
  initial #97 fall_read = last_fall;

  always #5 clk = ~clk;
  // `n` is k once the clock has risen k times, at 5, 15, ..., 95.
  always @(posedge clk) n <= n + 1;

  assert property (@(posedge clk) $past(n) == (n == 0 ? 0 : n - 1))
  else written_fails++;

  assert property (steps_declared) else declared_fails++;

  assert property (rises.steps_in_block) else block_fails++;

  assert property (@(posedge clk) steps_inferred(n))
  else inferred_written_fails++;

  // At the k-th fall, at 10k, `n` is k, so a fall earlier it was k - 1.
  assert property (steps_inferred(n)) inferred_default_passes++;
  else inferred_default_fails++;

  assert property (past_at_the_instance(n)) else instance_clock_fails++;

  // The action block counts ticks of the same list.
  assert property (@(posedge clk) steps_listed(n)) begin
    listed_checked++;
    if ($past(n) != ($sampled(n) == 0 ? 0 : $sampled(n) - 1)) listed_action_wrong++;
  end
  else listed_fails++;

  // Of two clocking events written one against the other, the second is in
  // force (LRM 16.13.3).
  assert property (@(negedge clk) @(posedge clk) $past(n) == (n == 0 ? 0 : n - 1))
  else juxtaposed_fails++;

  // At 32 the latest fall is at 30, where `n` was 3.
  initial #32 from_function = last_falling();

  // The action runs after the tick's updates, so `n` is read through
  // `$sampled` to compare against the value the assertion sampled.
  assert property (@(posedge clk) n >= 0) begin
    action_checked++;
    if ($past(n) != ($sampled(n) == 0 ? 0 : $sampled(n) - 1)) action_wrong++;
  end

  initial #102 $finish;

  final begin
    if (written_fails !== 0)
      $fatal(1, "a clock written on the statement failed %0d times",
             written_fails);
    if (declared_fails !== 0)
      $fatal(1, "a clock declared with the property failed %0d times",
             declared_fails);
    if (block_fails !== 0)
      $fatal(1, "a property declared in a clocking block failed %0d times",
             block_fails);
    if (inferred_written_fails !== 0)
      $fatal(1, "'$inferred_clock' under a written clock failed %0d times",
             inferred_written_fails);
    // The clock falls at 10, 20, ..., 100.
    if (inferred_default_passes !== 10 || inferred_default_fails !== 0)
      $fatal(1, "'$inferred_clock' at the top passed %0d and failed %0d, expected 10 and 0",
             inferred_default_passes, inferred_default_fails);
    if (action_checked !== 10 || action_wrong !== 0)
      $fatal(1, "an action block checked %0d times and read wrongly %0d times, expected 10 and 0",
             action_checked, action_wrong);
    if (instance_clock_fails !== 0)
      $fatal(1, "'$inferred_clock' after another clock failed %0d times",
             instance_clock_fails);
    if (listed_fails !== 0 || listed_checked !== 10 || listed_action_wrong !== 0)
      $fatal(1, "a list holding '$inferred_clock' failed %0d times, checked %0d times, read wrongly %0d times, expected 0, 10 and 0",
             listed_fails, listed_checked, listed_action_wrong);
    if (juxtaposed_fails !== 0)
      $fatal(1, "two juxtaposed clocking events failed %0d times",
             juxtaposed_fails);
    if (from_function !== 3)
      $fatal(1, "a function body read %0d, expected 3", from_function);
    if (fall_read !== 9)
      $fatal(1, "the continuous assignment read %0d, expected 9", fall_read);
    $display("All checks passed");
  end
endmodule

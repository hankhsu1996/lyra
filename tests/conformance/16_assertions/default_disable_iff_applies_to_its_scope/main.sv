// A concurrent assertion whose property states no disable condition takes the
// one a `default disable iff` declares for its scope (LRM 16.15): the module's,
// a generate block's that overrides it inside the block, and none in an
// instance, which the declaration does not reach. A disable condition the
// assertion or its property states wins, and `disable iff (1'b0)` is how one
// cancels the default. `$inferred_disable` names the default in force where the
// assertion stands, however deep the instance whose formal it defaults, or
// false where none is (LRM 16.14.7).
//
// Every assertion here fails at every tick it is not disabled at, so what each
// counts is how many ticks its disable condition left it.
module Sub (input logic clk);
  property never_inferred(r = $inferred_disable);
    @(posedge clk) disable iff (r) 1'b0;
  endproperty

  int fails = 0;
  int inferred_fails = 0;
  assert property (@(posedge clk) 1'b0) else fails++;
  assert property (never_inferred) else inferred_fails++;
endmodule

module Top;
  logic clk = 0;
  // Held through the ticks at 5, 15, 25 and 35.
  logic rst = 1;
  logic idle = 0;
  // Held through the ticks at 5 up to 65.
  logic block_rst = 1;

  default disable iff rst;

  property never_unless_idle;
    @(posedge clk) disable iff (idle) 1'b0;
  endproperty

  property never_inferred(r = $inferred_disable);
    @(posedge clk) disable iff (r) 1'b0;
  endproperty

  // However deep the instance holding it, `$inferred_disable` is the default in
  // force where the assertion stands.
  property never_nested;
    never_inferred;
  endproperty

  int by_default = 0;
  int by_own = 0;
  int by_cancel = 0;
  int by_property = 0;
  int by_inferred = 0;
  int cover_hits = 0;

  always #5 clk = ~clk;
  initial #40 rst = 0;
  initial #70 block_rst = 0;

  assert property (@(posedge clk) 1'b0) else by_default++;
  assert property (@(posedge clk) disable iff (idle) 1'b0) else by_own++;
  assert property (@(posedge clk) disable iff (1'b0) 1'b0) else by_cancel++;
  assert property (never_unless_idle) else by_property++;
  assert property (never_inferred) else by_inferred++;
  cover property (@(posedge clk) 1'b1) cover_hits++;

  if (1) begin : inherits
    int fails = 0;
    assert property (@(posedge clk) 1'b0) else fails++;
  end

  if (1) begin : overrides
    default disable iff block_rst;
    int fails = 0;
    int nested_fails = 0;
    assert property (@(posedge clk) 1'b0) else fails++;
    assert property (never_nested) else nested_fails++;
  end

  Sub sub (clk);

  initial #100 $finish;

  final begin
    // The clock rises at 5, 15, ..., 95: ten ticks, the last six after `rst`
    // falls and the last three after `block_rst` does.
    if (by_default !== 6)
      $fatal(1, "the default disabled all but %0d ticks, expected 6", by_default);
    if (by_own !== 10)
      $fatal(1, "an assertion's own condition left %0d ticks, expected 10", by_own);
    if (by_cancel !== 10)
      $fatal(1, "'disable iff (1'b0)' left %0d ticks, expected 10", by_cancel);
    if (by_property !== 10)
      $fatal(1, "a property's own condition left %0d ticks, expected 10",
             by_property);
    if (by_inferred !== 6)
      $fatal(1, "'$inferred_disable' left %0d ticks, expected 6", by_inferred);
    if (cover_hits !== 6)
      $fatal(1, "the default left a cover %0d hits, expected 6", cover_hits);
    if (inherits.fails !== 6)
      $fatal(1, "a generate block inheriting the default left %0d ticks, expected 6",
             inherits.fails);
    if (overrides.fails !== 3)
      $fatal(1, "a generate block's own default left %0d ticks, expected 3",
             overrides.fails);
    if (overrides.nested_fails !== 3)
      $fatal(1, "'$inferred_disable' in a nested instance left %0d ticks, expected 3",
             overrides.nested_fails);
    if (sub.fails !== 10)
      $fatal(1, "an instance the default does not reach left %0d ticks, expected 10",
             sub.fails);
    if (sub.inferred_fails !== 10)
      $fatal(1, "'$inferred_disable' with no default left %0d ticks, expected 10",
             sub.inferred_fails);
    $display("All checks passed");
  end
endmodule

// A pending procedural assertion instance is withdrawn by a flush point its
// process reaches before the instance matures -- the process resuming after an
// event control, an `always_comb` run again, or the outermost scope of the
// process disabled -- by a `disable` of the assertion itself, and by
// `$assertkill` (LRM 16.14.6.2, 16.14.6.4, 20.11). Disabling a scope that is
// neither leaves it pending. Each assertion here fails at the tick it begins,
// so its fail count is how many of its instances matured.
module Top;
  logic clk = 0;
  logic a = 0;
  logic b = 0;
  logic not_c;
  logic c = 0;
  logic never = 0;
  logic same;

  int resumed_fails = 0;
  int comb_fails = 0;
  int label_fails = 0;
  int outer_fails = 0;
  int inner_fails = 0;
  int kill_fails = 0;

  always #5 clk = ~clk;

  // Ticks land at 5, 15, 25 and 35. At 15 `a` changes, and `b` follows it in
  // the same step: resuming for the second change withdraws the instance the
  // first queued before it matures, so one instance of the two matures.
  initial begin
    #15 a = 1;
    #0 b = 1;
  end
  always @(a or b) begin : resumed
    p_resumed: assert property (@(posedge clk) never) else resumed_fails++;
  end

  // `not_c` follows `c` through a continuous assignment, so a procedure reading
  // both runs between the two changes; running again withdraws what it queued
  // then. It runs once at time 0 as well, and that instance matures.
  assign not_c = !c;
  initial #25 c = 1;
  always_comb begin : comb
    same = c == not_c;
    p_comb: assert property (@(posedge clk) never) else comb_fails++;
  end

  // A `disable` of the assertion withdraws what it queued, at 15 and 25.
  always @(posedge clk) begin : labelled
    p_label: assert property (@(posedge clk) never) else label_fails++;
    if ($time == 15 || $time == 25) disable p_label;
  end

  // Disabling the procedure's outermost scope withdraws its queue, at 15.
  always @(posedge clk) begin : outer
    p_outer: assert property (@(posedge clk) never) else outer_fails++;
  end
  always @(posedge clk) begin
    #0;
    if ($time == 15) disable outer;
  end

  // Disabling a block the assertion stands in, which is not the outermost
  // scope, withdraws nothing.
  always @(posedge clk) begin : not_outer
    begin : inner
      p_inner: assert property (@(posedge clk) never) else inner_fails++;
      disable inner;
    end
  end

  // `$assertkill` at 15 withdraws what is queued and stops checking, so
  // nothing is queued at 25; `$asserton` at 30 lets the tick at 35 queue again.
  always @(posedge clk) begin : killed
    p_kill: assert property (@(posedge clk) never) else kill_fails++;
    if ($time == 15) $assertkill(0, Top.killed.p_kill);
  end
  initial #30 $asserton(0, Top.killed.p_kill);

  initial #40 $finish;

  final begin
    if (resumed_fails !== 1)
      $fatal(1, "%0d resumed instances matured, expected 1", resumed_fails);
    if (comb_fails !== 2)
      $fatal(1, "%0d comb instances matured, expected 2", comb_fails);
    if (label_fails !== 2)
      $fatal(1, "%0d labelled instances matured, expected 2", label_fails);
    if (outer_fails !== 3)
      $fatal(1, "%0d instances of a disabled scope matured, expected 3",
             outer_fails);
    if (inner_fails !== 4)
      $fatal(1, "%0d instances under an inner disable matured, expected 4",
             inner_fails);
    if (kill_fails !== 2)
      $fatal(1, "%0d killed instances matured, expected 2", kill_fails);
    $display("All checks passed");
  end
endmodule

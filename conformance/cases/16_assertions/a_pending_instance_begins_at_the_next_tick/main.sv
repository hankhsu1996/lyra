// @top: Top Late
// A pending procedural assertion instance matures in the Observed region of the
// time step it was queued in. Where that step carries a tick of the
// assertion's own clock, the attempt begins there; where it does not, the
// instance waits and begins at the next tick (LRM 16.14.6). So where the
// procedure runs on one clock and the assertion counts ticks of another, the
// attempts follow what the procedure queued, not the assertion's clock: every
// instance queued before a tick begins at that tick, however many there are.
// One that matures after its step's tick was already evaluated still begins at
// that tick, as a program's process queues it, woken only after the Observed
// region (LRM 4.4).
//
// A matured instance is no attempt yet, so a `disable iff` condition that comes
// true and false again before its tick does not reach it; one that holds at
// the tick disables the attempt as it begins (LRM 16.12, 16.14.6.4). An
// instance whose tick never comes simply never begins.
module Top;
  logic slow = 0;
  logic fast = 0;

  int slow_hits = 0;
  int fast_hits = 0;
  int fast_ticks = 0;

  logic go = 0;
  logic go_late = 0;
  logic rst = 0;
  logic rst_late = 0;
  logic ok = 0;
  logic never = 0;
  int between_fails = 0;
  int at_tick_fails = 0;
  int unticked_hits = 0;
  int late_hits = 0;

  // `fast` rises at 5, 15, 25, ...; `slow` at 10, 30, 50.
  always #5 fast = ~fast;
  always #10 slow = ~slow;

  always @(posedge fast) fast_ticks++;

  // Queued at each rising `slow`, a step `fast` has no edge in, so each waits
  // for the rising `fast` five units later.
  always @(posedge slow) begin
    on_fast: cover property (@(posedge fast) 1) slow_hits++;
  end

  // Queued at each rising `fast`. The rising `slow` at 10 finds the one queued
  // at 5, and the ones at 30 and 50 find the two queued since the last.
  always @(posedge fast) begin
    on_slow: cover property (@(posedge slow) 1) fast_hits++;
  end

  // Queued at 1; `rst` is high from 2 to 3 only, so the tick at 5 begins the
  // attempt and it fails.
  initial begin
    #1 go = 1;
    #1 rst = 1;
    #1 rst = 0;
  end
  always @(posedge go) begin
    between: assert property (@(posedge fast) disable iff (rst) ok)
      else between_fails++;
  end

  // Queued at 21; `rst_late` is high from 24 to 26, over the tick at 25, so
  // the attempt is disabled as it begins.
  initial begin
    #21 go_late = 1;
    #3 rst_late = 1;
    #2 rst_late = 0;
  end
  always @(posedge go_late) begin
    at_tick: assert property (@(posedge fast) disable iff (rst_late) ok)
      else at_tick_fails++;
  end

  // Queued at 30 on a clock that never ticks.
  always @(posedge slow) begin
    if ($time == 30) unticked: cover property (@(posedge never) 1) unticked_hits++;
  end

  initial #56 $finish;

  final begin
    if (fast_ticks !== 6)
      $fatal(1, "fast rose %0d times, expected 6", fast_ticks);
    // Queued at 10, 30 and 50; each begins at the next rising `fast`.
    if (slow_hits !== 3)
      $fatal(1, "%0d attempts on fast, expected 3", slow_hits);
    if (fast_hits !== 5)
      $fatal(1, "%0d attempts on slow, expected 5", fast_hits);
    if (between_fails !== 1)
      $fatal(1, "%0d failures past a disable between ticks, expected 1",
             between_fails);
    if (at_tick_fails !== 0)
      $fatal(1, "%0d failures disabled at the tick, expected 0", at_tick_fails);
    if (unticked_hits !== 0)
      $fatal(1, "%0d attempts on a clock that never ticks", unticked_hits);
    // Queued at 5, 15, 25, 35, 45 and 55, each after its tick was evaluated.
    if (late_hits !== 6)
      $fatal(1, "%0d attempts queued after their tick, expected 6", late_hits);
    $display("All checks passed");
  end
endmodule

// Woken by each rising `fast` only after the Observed region that evaluated it.
program Late;
  initial forever begin
    @(posedge Top.fast);
    late: cover property (@(posedge Top.fast) 1) Top.late_hits++;
  end
endprogram

// A clocking event is an event expression (LRM 16.5), and an event expression
// may name an event, whose ticks are its triggers (LRM 15.5.2). So a concurrent
// assertion, a sampled value function and an `expect` all take a named event as
// their clock, and each tick samples in the Preponed region of the time step
// the trigger happens in, exactly as an edge of a signal does.
//
// A sampled value function's gate qualifies the event it counts (LRM 16.9.3),
// so a trigger where the gate does not hold is no prior tick, whatever the
// clock is.
module Top;
  event tick;
  int v = 0;
  bit ok = 1;
  bit en = 1;

  int passes = 0;
  int fails = 0;
  int ticks = 0;
  int ungated;
  int gated;
  bit expected = 0;

  a_on_tick: assert property (@(tick) ok)
      passes = passes + 1;
    else
      fails = fails + 1;

  always @(tick) begin
    ticks = ticks + 1;
    if (ticks == 4) begin
      ungated = $past(v, 1, , @(tick));
      gated = $past(v, 1, en, @(tick));
    end
  end

  // Triggers land at 10, 20, 30 and 40. Every write happens between two of
  // them, so each tick samples a settled value: `v` is 1, 2, 3, 4; `ok` is 1,
  // 0, 1, 1; `en` is 1, 1, 0, 1.
  initial begin
    #5 v = 1;
    #5 -> tick;
    #5 begin
      v = 2;
      ok = 0;
    end
    #5 -> tick;
    #5 begin
      v = 3;
      ok = 1;
      en = 0;
    end
    #5 -> tick;
    #5 begin
      v = 4;
      en = 1;
    end
    #5 -> tick;
    #5 $finish;
  end

  // Its first tick is the trigger at 20, and the one after it at 30 samples
  // `ok` as 1.
  initial begin
    #12 expect (@(tick) ##1 ok) expected = 1;
  end

  final begin
    if (passes !== 3) $fatal(1, "the property held at %0d ticks, expected 3",
                             passes);
    if (fails !== 1) $fatal(1, "the property failed at %0d ticks, expected 1",
                            fails);
    if (ticks !== 4) $fatal(1, "the clock reached %0d ticks, expected 4", ticks);
    if (ungated !== 3)
      $fatal(1, "the prior tick's value was %0d, expected 3", ungated);
    if (gated !== 2)
      $fatal(1, "the prior gated tick's value was %0d, expected 2", gated);
    if (expected !== 1) $fatal(1, "the expect did not hold");
    $display("All checks passed");
  end
endmodule

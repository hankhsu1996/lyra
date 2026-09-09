// A value change function compares the sampled value of its expression with
// the sampled value at the most recent strictly prior tick of a clocking event
// (LRM 16.9.3). Neither operand is the value the variable holds where the call
// stands: both are Preponed values (LRM 16.5.1), and the prior one belongs to a
// time step that has already ended.
//
// No call here writes a clocking event. Each is inferred from the procedure it
// sits in (LRM 16.14.6): the block has no blocking timing control, exactly one
// event control, and the terms of that event expression are read nowhere else
// in the body -- so `posedge clk` is the clock all four are read against.
//
// The first tick has no strictly prior tick, so the comparison is against the
// default sampled value (LRM 16.9.3), which for a static variable is the value
// its declaration assigns (LRM 16.5.1). `d` declares 0, so the first tick sees
// no change rather than an unknown one.
module Top;
  logic clk = 0;
  logic d = 0;

  int ticks = 0;
  bit rose_at[4];
  bit fell_at[4];
  bit stable_at[4];
  bit changed_at[4];

  always #5 clk = ~clk;

  always @(posedge clk) begin
    ticks = ticks + 1;
    if (ticks <= 4) begin
      rose_at[ticks-1] = $rose(d);
      fell_at[ticks-1] = $fell(d);
      stable_at[ticks-1] = $stable(d);
      changed_at[ticks-1] = $changed(d);
    end
  end

  // Ticks land at 5, 15, 25 and 35. Each write to `d` happens between two of
  // them, so what each tick compares is one settled value against the previous.
  initial begin
    #8 d = 1;
    #20 d = 0;
    #20 $finish;
  end

  // `d` is written 1 at time 8 and 0 at time 28, so the ticks at 15 and 35 are
  // the ones a change reaches.
  bit expect_rose[4] = '{1'b0, 1'b1, 1'b0, 1'b0};
  bit expect_fell[4] = '{1'b0, 1'b0, 1'b0, 1'b1};
  bit expect_stable[4] = '{1'b1, 1'b0, 1'b1, 1'b0};
  bit expect_changed[4] = '{1'b0, 1'b1, 1'b0, 1'b1};

  final begin
    if (ticks < 4) $fatal(1, "the clock reached %0d ticks, expected 4", ticks);
    for (int t = 0; t < 4; t++) begin
      if (rose_at[t] !== expect_rose[t])
        $fatal(1, "tick %0d: $rose was %b, expected %b", t + 1, rose_at[t],
               expect_rose[t]);
      if (fell_at[t] !== expect_fell[t])
        $fatal(1, "tick %0d: $fell was %b, expected %b", t + 1, fell_at[t],
               expect_fell[t]);
      if (stable_at[t] !== expect_stable[t])
        $fatal(1, "tick %0d: $stable was %b, expected %b", t + 1, stable_at[t],
               expect_stable[t]);
      if (changed_at[t] !== expect_changed[t])
        $fatal(1, "tick %0d: $changed was %b, expected %b", t + 1,
               changed_at[t], expect_changed[t]);
    end
    $display("All checks passed");
  end
endmodule

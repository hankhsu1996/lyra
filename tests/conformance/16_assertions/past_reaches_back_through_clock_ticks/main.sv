// `$past(e, k)` answers with the sampled value of `e` at the kth strictly prior
// tick of a clocking event, and with `e`'s default sampled value where that
// many prior ticks do not exist (LRM 16.9.3). `k` defaults to 1 and is an
// elaboration-time constant. The clocking event is inferred from the procedure
// (LRM 16.14.6), as it is for the value change functions.
//
// The default sampled value of a static variable is the value its declaration
// assigns (LRM 16.5.1), which is what separates "no such tick" from every real
// answer here: `v` is declared 7 and never holds 7 again, so an answer of 7 can
// only be the default.
module Top;
  logic clk = 0;
  int v = 7;

  int ticks = 0;
  int past1_at[5];
  int past2_at[5];
  int past3_at[5];

  always #5 clk = ~clk;

  // Moves `v` on every tick. A reader in another procedure woken by the same
  // edge sees the value from before this tick either way, so which of the two
  // the Active region runs first does not matter (LRM 16.5.1).
  always @(posedge clk) v = v + 10;

  always @(posedge clk) begin
    ticks = ticks + 1;
    if (ticks <= 5) begin
      past1_at[ticks-1] = $past(v);
      past2_at[ticks-1] = $past(v, 2);
      past3_at[ticks-1] = $past(v, 3);
    end
  end

  initial begin
    v = 3;
    #52 $finish;
  end

  // Ticks land at 5, 15, 25, 35 and 45, and the sampled value of `v` at them is
  // 3, 13, 23, 33 and 43 -- the value from before each tick moved it.
  int expect_past1[5] = '{7, 3, 13, 23, 33};
  int expect_past2[5] = '{7, 7, 3, 13, 23};
  int expect_past3[5] = '{7, 7, 7, 3, 13};

  final begin
    if (ticks < 5) $fatal(1, "the clock reached %0d ticks, expected 5", ticks);
    for (int t = 0; t < 5; t++) begin
      if (past1_at[t] !== expect_past1[t])
        $fatal(1, "tick %0d: $past(v) was %0d, expected %0d", t + 1,
               past1_at[t], expect_past1[t]);
      if (past2_at[t] !== expect_past2[t])
        $fatal(1, "tick %0d: $past(v, 2) was %0d, expected %0d", t + 1,
               past2_at[t], expect_past2[t]);
      if (past3_at[t] !== expect_past3[t])
        $fatal(1, "tick %0d: $past(v, 3) was %0d, expected %0d", t + 1,
               past3_at[t], expect_past3[t]);
    end
    $display("All checks passed");
  end
endmodule

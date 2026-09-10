// A sequence and a property may be declared with formal arguments and
// instantiated with actuals substituted for them, and the behavior of the
// instance is that of the body with the substitution performed (LRM 16.8,
// 16.12). A named sequence carrying no clocking event of its own inherits the
// one the instantiating property states.
//
// The same property is instantiated twice, once with two different signals and
// once with the same signal in both positions, so what the case pins down is
// that each actual reaches the formal it was written against rather than that
// the property was evaluated at all.
module Top;
  logic clk = 0;
  logic req = 0;
  logic gnt = 0;

  int passes = 0;
  int fails = 0;
  int same_signal_fails = 0;

  always #5 clk = ~clk;

  sequence s_pulse(sig);
    sig ##1 !sig;
  endsequence

  property p_follows(trig, sig);
    @(posedge clk) trig |=> s_pulse(sig);
  endproperty

  // Ticks land at 5, 15, 25, 35 and 45. `req` is sampled high only at the tick
  // at 15, and `gnt` only at the tick at 25.
  initial begin
    #8;
    req = 1;
    #10;
    req = 0;
    gnt = 1;
    #10;
    gnt = 0;
    #20 $finish;
  end

  a_named: assert property (p_follows(req, gnt))
      passes = passes + 1;
    else
      fails = fails + 1;

  a_same_signal: assert property (p_follows(req, req))
    else
      same_signal_fails = same_signal_fails + 1;

  final begin
    // The one attempt whose antecedent matches reads `gnt` high at the tick at
    // 25 and low at the tick at 35; the other four succeed vacuously.
    if (fails !== 0)
      $fatal(1, "the property failed %0d times, expected none", fails);
    if (passes !== 5)
      $fatal(1, "the property held at %0d attempts, expected 5", passes);
    // With `req` substituted for both formals the pulse is looked for where
    // `req` is already low, so that attempt is false.
    if (same_signal_fails !== 1)
      $fatal(1, "the same-signal instance failed %0d times, expected 1",
             same_signal_fails);
    $display("All checks passed");
  end
endmodule

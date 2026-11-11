// Before the ticks a sampled value function reaches exist, LRM 16.9.3 answers
// with "the default sampled value of expression1", whose recursive definition
// gives an automatic variable its type's default, as does its rule that a
// value change function at the first tick compares with the default sampled
// value, while LRM 16.5.1 says that for a past value of an automatic variable "the current value of the
// automatic variable is taken instead", and the same of a constant cast. The
// standard does not say which governs. Lyra reads an automatic variable and a
// constant cast as they are now at every tick, the first included, so at the
// first tick `$past(b[i])` is the default of `b` at the current `i`. This case
// holds Lyra to that.
module Top;
  localparam logic [3:0] kStart = 4'b0110;

  bit clk;
  logic [3:0] b = kStart;
  int checks = 0;
  int failures = 0;

  always #5 clk = !clk;

  // Only the first tick is checked; it has no strictly prior tick.
  initial begin
    @(posedge clk);
    for (int i = 0; i < 4; i++) begin
      checks += 4;
      if ($past(b[i], 1, , @(posedge clk)) !== kStart[i]) failures++;
      if ($past(i, 1, , @(posedge clk)) != i) failures++;
      if ($stable(i, @(posedge clk)) !== 1'b1) failures++;
      if ($past(b[const'(i)], 1, , @(posedge clk)) !== kStart[i]) failures++;
    end
    $finish;
  end

  final begin
    if (checks != 16) $fatal(1, "%0d checks ran, expected 16", checks);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

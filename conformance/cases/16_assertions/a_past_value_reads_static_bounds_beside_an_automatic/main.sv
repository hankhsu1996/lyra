// A sampled value function reads its operand where the call is written, each
// static variable as of the tick it names and each automatic variable as it is
// now (LRM 16.5.1, 16.9.3). That holds of a static variable bounding a value
// range an automatic is tested against (LRM 11.4.13), and of one in a queue
// index counted from `$` (LRM 7.10.1) where the queue is automatic: the bound
// and the index are the past values, and the automatic is the current one.
module Top;
  bit clk;
  int lo = 0;
  int hi = 10;
  int s = 0;
  int checks = 0;
  int failures = 0;

  // What each static variable was at each tick, the latest last.
  int lo_seen[$];
  int s_seen[$];

  always #5 clk = !clk;

  // Each value changes between two ticks.
  always @(negedge clk) begin
    lo = (lo == 0) ? 6 : 0;
    s = (s + 1) % 3;
  end

  always @(posedge clk) begin
    automatic int k = 5;
    automatic int qa[$] = '{10, 20, 30};
    lo_seen.push_back(lo);
    s_seen.push_back(s);
    // From the second tick on, every read below reaches a tick that happened.
    if (lo_seen.size() > 1) begin
      checks++;
      if ($past(k inside {[lo:hi]}) !== (k >= lo_seen[$-1] && k <= hi)) begin
        failures++;
        $display("tick %0d: $past(k inside {[lo:hi]}) was wrong", lo_seen.size());
      end
      checks++;
      if ($past(qa[$ - s]) !== qa[2 - s_seen[$-1]]) begin
        failures++;
        $display("tick %0d: $past(qa[$ - s]) was %0d, expected %0d",
                 s_seen.size(), $past(qa[$ - s]), qa[2 - s_seen[$-1]]);
      end
    end
  end

  initial #50 $finish;

  final begin
    if (checks != 8) $fatal(1, "%0d checks ran, expected 8", checks);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

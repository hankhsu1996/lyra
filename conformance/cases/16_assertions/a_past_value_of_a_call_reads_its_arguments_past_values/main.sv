// The sampled value of an expression holding a function call is the function
// applied to the sampled values of its arguments (LRM 16.5.1), so a past value
// of an operation that reads only its operands -- `$countones`, a queue's
// `size()` -- is that operation over the operands' values at the tick named
// (LRM 16.9.3).
module Top;
  bit clk;
  logic [7:0] w = 8'h0f;
  int q[$] = '{1, 2};

  logic [7:0] w_seen[$];
  int size_seen[$];
  int checks = 0;
  int failures = 0;

  always #5 clk = !clk;
  always @(posedge clk) w <= w << 1;
  // It changes between two ticks, so a read at a tick sees it settled.
  always @(negedge clk) q.push_back(0);

  task automatic check(int got, int want, string what);
    checks++;
    if (got !== want) begin
      failures++;
      $display("tick %0d: %s was %0d, expected %0d", w_seen.size(), what, got,
               want);
    end
  endtask

  always @(posedge clk) begin
    w_seen.push_back(w);
    size_seen.push_back(q.size());
    check($past($countones(w)),
          $countones(w_seen.size() > 1 ? w_seen[$-1] : 8'h0f),
          "$past($countones(w))");
    check($past(q.size()), size_seen.size() > 1 ? size_seen[$-1] : 2,
          "$past(q.size())");
  end

  initial #48 $finish;

  final begin
    if (checks != 5 * 2)
      $fatal(1, "%0d checks ran, expected %0d", checks, 5 * 2);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

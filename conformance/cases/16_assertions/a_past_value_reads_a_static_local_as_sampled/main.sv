// A variable a procedure or a subroutine declares without `automatic` is
// static (LRM 6.21), and so is every formal of a static subroutine (LRM
// 13.3.2): it has one value for the whole run, so a sampled value function
// reads it as of the tick it names, as it reads a module variable (LRM 16.5.1,
// 16.9.3). That holds wherever it stands in the call: as the operand, as an
// index into another variable, in the gate of `$past`, and in a concurrent
// assertion of the procedure, which reads it sampled rather than saved (LRM
// 16.14.6.1).
module Top;
  localparam logic [3:0] kStart = 4'b1001;

  bit clk;
  logic [3:0] b = kStart;
  int cnt = 0;

  default clocking @(posedge clk); endclocking

  // What each value was at each tick, the latest last.
  logic [3:0] b_seen[$];
  int cnt_seen[$];
  int s_seen[$];
  int checks = 0;
  int failures = 0;
  int passes = 0;
  int fails = 0;
  // What `$past(b[k])` is at the next tick, where `k` is the procedure's
  // static loop variable: b at this tick, at the index `k` held as this tick
  // was sampled. Its declaration is the answer before any tick.
  logic want = kStart[0];

  always #5 clk = !clk;
  always @(posedge clk) begin
    b <= b + 4'd5;
    cnt <= cnt + 1;
  end

  function int formal_past(int a);
    return $past(a);
  endfunction

  function int local_past();
    static int kept;
    int answer;
    answer = $past(kept);
    kept = cnt;
    return answer;
  endfunction

  // A value sampled at the tick `n` back from the latest of `seen`, or the
  // default sampled value `start` before that tick.
  function automatic int at(int seen[$], int n, int start);
    return seen.size() > n ? seen[seen.size()-1-n] : start;
  endfunction

  task automatic check(int got, int want_value, string what);
    checks++;
    if (got !== want_value) begin
      failures++;
      $display("tick %0d: %s was %0d, expected %0d", b_seen.size(), what, got,
               want_value);
    end
  endtask

  always @(posedge clk) begin
    int s;
    int k;
    // A static variable read here holds what the last step left it, which is
    // its sampled value.
    b_seen.push_back(b);
    cnt_seen.push_back(cnt);
    s_seen.push_back(s);
    check($past(s), at(s_seen, 1, 0), "$past(s)");
    check($past(b[s]),
          b_seen.size() > 1 ? b_seen[$-1][s_seen[$-1]] : kStart[0],
          "$past(b[s])");
    check(formal_past(cnt), at(cnt_seen, 2, 0),
          "$past of a static function's formal");
    check(local_past(), at(cnt_seen, 2, 0),
          "$past of a static function's local");
    s = (s + 1) % 4;
    want = b[k];
    for (k = 0; k < 2; k++)
      a: assert property ($past(b[k]) === want) passes++; else fails++;
  end

  // The gate reads a static variable of this procedure, which changes only
  // between two ticks.
  logic [3:0] gated[$];
  initial begin
    static bit en = 0;
    forever begin
      @(posedge clk);
      gated.push_back($past(b, 1, en, @(posedge clk)));
      #2 en = !en;
    end
  end

  initial #88 $finish;

  final begin
    if (b_seen.size() != 9)
      $fatal(1, "the clock ticked %0d times, expected 9", b_seen.size());
    // `en` holds at the second tick, the fourth, and every second one after.
    for (int n = 1; n <= gated.size(); n++) begin
      logic [3:0] expected;
      expected = n <= 2 ? kStart : b_seen[(n - 1) / 2 * 2 - 1];
      if (gated[n-1] !== expected)
        $fatal(1, "tick %0d: $past(b, 1, en) was %b, expected %b", n,
               gated[n-1], expected);
    end
    if (passes != 2 * 9 || fails != 0)
      $fatal(1, "the assertion passed %0d times and failed %0d", passes,
             fails);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

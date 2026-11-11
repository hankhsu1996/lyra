// LRM 16.5.1 says a function in an expression is called "on the sampled values
// of its arguments at the time of the expression evaluation", and LRM 16.9.3
// says `$past` returns "the sampled value of expression1 in a particular time
// step strictly prior" to the current one. The standard does not say whether
// the call of a past value is made where the past value is read, over its
// arguments' past values, or was made at that earlier time step. The two
// differ only for a function whose answer follows what no argument names: a
// variable it reads by its own name, or `$time`. Lyra makes the call where the
// past value is read, so such a function reads that variable as it is now,
// and `$past($time)` is the time of the read. This case holds Lyra to that.
module Top;
  bit clk;
  int cnt = 10;
  int g = 100;

  int cnt_seen[$];
  int checks = 0;
  int failures = 0;

  always #5 clk = !clk;
  always @(posedge clk) begin
    cnt <= cnt + 1;
    g <= g + 100;
  end

  function automatic int plus_g(int v);
    return v + g;
  endfunction

  task automatic check(int got, int want, string what);
    checks++;
    if (got !== want) begin
      failures++;
      $display("tick %0d: %s was %0d, expected %0d", cnt_seen.size(), what,
               got, want);
    end
  endtask

  always @(posedge clk) begin
    cnt_seen.push_back(cnt);
    check($past(plus_g(cnt)), (cnt_seen.size() > 1 ? cnt_seen[$-1] : 10) + g,
          "$past(plus_g(cnt))");
    check(int'($past($time)), int'($time), "$past($time)");
  end

  initial #48 $finish;

  final begin
    if (checks != 5 * 2)
      $fatal(1, "%0d checks ran, expected %0d", checks, 5 * 2);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule

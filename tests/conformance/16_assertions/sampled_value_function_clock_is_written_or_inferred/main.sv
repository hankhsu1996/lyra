// Which clocking event a sampled value function samples against is either
// written at the call or found from where the call stands (LRM 16.9.3). Two of
// the five ordered rules reach a call outside an assertion: the clock inferred
// from the procedure (LRM 16.14.6), and otherwise the scope's default clocking
// (LRM 14.12). This states the other two ways -- a written event, and the
// default clocking -- against one variable, so the answers differ only by which
// event each read counted ticks of.
//
// The procedure here can infer nothing: LRM 16.14.6 requires a procedure with
// no blocking timing control, and this one delays. So the read that writes no
// event falls to the default clocking, and the read that writes one uses it.
module Top;
  logic fast = 0;
  logic slow = 0;
  int v = 7;

  int by_written_event;
  int by_default_clocking;

  default clocking cb @(posedge slow);
  endclocking

  always #5 fast = ~fast;
  always #10 slow = ~slow;

  always @(posedge fast) v = v + 10;

  initial begin
    v = 3;
    #32;
    // `$past`'s optional arguments are positional, so the tick count and the
    // gating expression are elided by their commas to reach the event (LRM
    // 16.9.3).
    by_written_event = $past(v, , , @(posedge fast));
    by_default_clocking = $past(v);
    #10 $finish;
  end

  final begin
    // `fast` rises at 5, 15, 25 and 35, and `v` moves at each; `slow` rises at
    // 10, 30 and 50. Reading at time 32, the most recent strictly prior tick is
    // time 25 for `fast`, where `v` had not yet been moved past 23, and time 30
    // for `slow`, where it stood at 33.
    if (by_written_event !== 23)
      $fatal(1, "the read naming its own event was %0d, expected 23",
             by_written_event);
    if (by_default_clocking !== 33)
      $fatal(1, "the read falling to the default clocking was %0d, expected 33",
             by_default_clocking);
    $display("All checks passed");
  end
endmodule

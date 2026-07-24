// LRM 12.4.2.1 report lifetime. Each detected violation is scheduled as its own
// pending report, so one execution reaching a check twice reports twice, and
// waiting for time to pass is not a flush point -- both survive the `#0`.
// Resuming from a wait on a condition is one: Glitch's check runs first against
// a stale `na` and violates, `na` settling retriggers the block, and that resume
// discards the pending report, so the settled re-run leaves nothing to report.
module Timing;
  function automatic void chk(int v);
    unique if (v > 0) ;
    else if (v > 3) ;
  endfunction
  initial begin
    for (int i = 0; i < 2; i++) chk(5);
    #0;
  end
endmodule

module Glitch;
  bit a;
  bit na;
  int z;
  always_comb begin
    unique if (a) z = 1;
    else if (na) z = 2;
  end
  always_comb na = !a;
endmodule

module Top;
  Timing t();
  Glitch g();
endmodule

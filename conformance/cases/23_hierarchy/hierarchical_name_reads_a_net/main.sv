// A net is one of the declaration kinds LRM 23.8 lists, so a hierarchical name
// reads one as it reads a variable, and a process waits on one the same way
// (LRM 23.6). The value a single-driver net carries is what its driver
// contributes (LRM 6.6), which is what the reader observes.
module Child;
  logic driven = 1'b0;
  wire signalled;
  assign signalled = driven;
endmodule

module Top;
  Child c ();

  logic saw = 1'b0;
  int woke = 0;

  always @(c.signalled) woke = woke + 1;

  initial begin
    #1;
    c.driven = 1'b1;
    #1;
    saw = c.signalled;
  end

  final begin
    if (saw !== 1'b1) $fatal(1, "a net read %b, expected 1", saw);
    if (woke === 0) $fatal(1, "waiting on a net never woke");
    $display("All checks passed");
  end
endmodule

// The same net, reached the other way. A downward name reads a net today and
// an upward one does not, which is the one place direction still decides
// whether a declaration kind is reachable -- LRM 23.8 lists a net among the
// items an upward reference names, so the two directions owe the same answer.
module Child;
  logic saw = 1'b0;

  initial begin
    #1;
    saw = Top.signalled;
  end
endmodule

module Top;
  wire signalled;
  assign signalled = 1'b1;

  Child c ();

  final begin
    if (c.saw !== 1'b1) $fatal(1, "an upward net read %b, expected 1", c.saw);
    $display("All checks passed");
  end
endmodule

// A ref port is not a second object joined to the first by a continuous
// assignment: references to the port are references to the variable it is
// connected to in its instantiation, which is also why such a port cannot be
// left unconnected (LRM 23.3.3.2). A write inside the child is therefore a
// write to the parent's variable and is seen with no delay by a procedure
// sensitive to that variable; a read in the child's own declaration initializer
// reaches the connected variable, which holds the default initial value of its
// data type while nothing has assigned it (LRM 6.8); and a ref port handed on
// as the connection for a ref port one level down still denotes the one
// variable at the end of the chain rather than the port in between.
module Probe(ref logic [7:0] pr);
  logic [7:0] captured = pr;
  bit saw_default = (pr === 8'hxx);

  initial #1 pr = 8'ha5;
endmodule

module Bumper(ref int r);
  initial begin
    #2;
    r = r + 100;
  end
endmodule

module Leaf(ref int lr);
  initial #1 lr = lr + 50;
endmodule

module Mid(ref int mr);
  Leaf leaf(.lr(mr));
endmodule

module Top;
  logic [7:0] probed;
  int shared;
  int mirror;
  int chain = 7;

  Probe probe(.pr(probed));
  Bumper bumper(.r(shared));
  Mid mid(.mr(chain));

  always @(shared) mirror = shared;

  initial begin
    shared = 5;
    #1 shared = 9;
  end

  final begin
    if (probe.captured !== 8'hxx)
      $fatal(1, "probe.captured was %h, expected xx", probe.captured);
    if (probe.saw_default !== 1'b1)
      $fatal(1, "probe.saw_default was %b, expected 1", probe.saw_default);
    if (probed !== 8'ha5)
      $fatal(1, "probed was %h, expected a5", probed);
    if (shared !== 109) $fatal(1, "shared was %0d, expected 109", shared);
    if (mirror !== 109) $fatal(1, "mirror was %0d, expected 109", mirror);
    if (chain !== 57) $fatal(1, "chain was %0d, expected 57", chain);
    $display("All checks passed");
  end
endmodule

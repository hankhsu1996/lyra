// A hierarchical name's first segment is searched for first at the current
// level and then in higher-level modules until it is found (LRM 23.9), and a
// local path takes precedence over the path from the top (LRM 23.3.1). So
// `Top.u` written in `Child` reaches `X.Top.u`, the instance the enclosing
// module `X` declares, though a top-level instance is also named `Top` and
// declares a `u` of its own; and a timescale function given that name answers
// for the design element it reaches (LRM 20.4.1).
`timescale 1ns / 1ns

module Child;
  initial
    if ($timeunit(Top.u) !== -6 || $timeprecision(Top.u) !== -9)
      $fatal(1, "Top.u gave %0d / %0d, expected -6 / -9", $timeunit(Top.u),
             $timeprecision(Top.u));
endmodule

module Leaf;
  timeunit 1us / 1ns;
endmodule

module Y;
  Leaf u ();
endmodule

module X;
  Y Top ();
  Child c ();
endmodule

module Top;
  timeunit 1ms / 1ns;
  int u;
  X x ();

  initial #1 $display("All checks passed");
endmodule

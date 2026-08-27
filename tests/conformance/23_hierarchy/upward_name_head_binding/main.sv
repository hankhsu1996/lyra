// The head of an upward reference may be the name of a module rather than of
// an instance: the name of a module above the reference identifies that module
// and its place in the hierarchy, so the reference reaches the enclosing
// instance of that module wherever in the tree that instance happens to sit,
// and not only when the module is the one at the root (LRM 23.8). Where a
// module name and an instance name are spelled alike, the search still runs
// outward level by level, and precedence is given to the instance name
// (LRM 23.9). Every candidate here holds a different value, so a reference
// that bound to the wrong one would be visible.
module DutProbe;
  int seen;

  always_comb seen = Dut.s;
endmodule

module Dut;
  int s;
  DutProbe dp();
endmodule

module Probe;
  int seen;

  always_comb seen = Alias.s;
endmodule

module Cell;
  int s;
  Probe p();
endmodule

module Holder;
  Cell Alias();
endmodule

module Alias;
  int s;
  Holder h();
endmodule

module Top;
  Dut u();
  Alias outer();

  initial begin
    u.s = 41;
    outer.s = 100;
    outer.h.Alias.s = 7;
  end

  final begin
    if (u.dp.seen !== 41)
      $fatal(1, "Dut.s seen from Top.u.dp was %0d, expected 41", u.dp.seen);
    if (outer.h.Alias.p.seen !== 7)
      $fatal(1, "Alias.s seen from Top.outer.h.Alias.p was %0d, expected 7",
             outer.h.Alias.p.seen);
    $display("All checks passed");
  end
endmodule

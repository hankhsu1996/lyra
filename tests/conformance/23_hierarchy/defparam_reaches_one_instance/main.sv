// A defparam changes a parameter of the one instance its hierarchical name
// reaches (LRM 23.10.1), and takes precedence over what that instance's own
// instantiation assigned (LRM 23.10). Every other instance of the same module
// keeps its own value, even where the two sit under parents built from one
// module: one or more levels down, through a loop generate's block, where the
// value decides which generate alternative stands
// (LRM 27.5), and where it sizes a declaration rather than only being read.
// A parameter changed by a defparam and one changed by the instantiation's own
// assignment hold the same value the same way.
module Leaf #(parameter int K = 5, parameter int W = 4);
  int k = K;
  logic [W-1:0] w;
  int width = $bits(w);
endmodule

module Mid;
  Leaf l ();
endmodule

module Outer;
  Mid m ();
endmodule

module Looped;
  for (genvar i = 0; i < 2; i++) begin : g
    Leaf u ();
  end
endmodule

module Sel #(parameter int K = 5);
  int arm = 0;
  if (K == 5) begin : d
    initial arm = 1;
  end else begin : o
    initial arm = 2;
  end
endmodule

module Chooser;
  Sel s ();
endmodule

// A defparam written inside a module reaches that module's own child, the same
// in every instance of it.
module SelfSet;
  Leaf l ();
  defparam l.K = 7;
endmodule

module Top;
  Mid m1 (), m2 ();
  defparam m1.l.K = 9;

  Mid wide1 (), wide2 ();
  defparam wide2.l.W = 12;

  Outer o1 (), o2 ();
  defparam o2.m.l.K = 11;

  Looped x (), y ();
  defparam y.g[1].u.K = 13;

  Chooser c1 (), c2 ();
  defparam c2.s.K = 9;

  SelfSet s1 (), s2 ();

  Leaf #(.K(17)) assigned ();
  Leaf overridden ();
  defparam overridden.K = 17;

  final begin
    if (m1.l.k !== 9) $fatal(1, "m1.l.K was %0d", m1.l.k);
    if (m2.l.k !== 5) $fatal(1, "m2.l.K was %0d", m2.l.k);
    if (wide1.l.width !== 4) $fatal(1, "wide1.l is %0d bits", wide1.l.width);
    if (wide2.l.width !== 12) $fatal(1, "wide2.l is %0d bits", wide2.l.width);
    if (o1.m.l.k !== 5) $fatal(1, "o1.m.l.K was %0d", o1.m.l.k);
    if (o2.m.l.k !== 11) $fatal(1, "o2.m.l.K was %0d", o2.m.l.k);
    if (x.g[0].u.k !== 5 || x.g[1].u.k !== 5)
      $fatal(1, "x's blocks read %0d and %0d", x.g[0].u.k, x.g[1].u.k);
    if (y.g[0].u.k !== 5) $fatal(1, "y.g[0].u.K was %0d", y.g[0].u.k);
    if (y.g[1].u.k !== 13) $fatal(1, "y.g[1].u.K was %0d", y.g[1].u.k);
    if (c1.s.arm !== 1) $fatal(1, "c1.s took arm %0d", c1.s.arm);
    if (c2.s.arm !== 2) $fatal(1, "c2.s took arm %0d", c2.s.arm);
    if (s1.l.k !== 7 || s2.l.k !== 7)
      $fatal(1, "s1.l.K was %0d and s2.l.K %0d", s1.l.k, s2.l.k);
    if (assigned.k !== 17) $fatal(1, "assigned.K was %0d", assigned.k);
    if (overridden.k !== 17) $fatal(1, "overridden.K was %0d", overridden.k);
    $display("All checks passed");
  end
endmodule

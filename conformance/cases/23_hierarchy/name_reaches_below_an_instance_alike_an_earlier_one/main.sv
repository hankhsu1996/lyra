// Every instance of a module is a copy of its own, however alike two are
// written (LRM 23.3), and a hierarchical name reaches one of them by its path
// (LRM 23.6). So a name reaching into an instance written alike an earlier one
// reads that one's own variables, at every depth below it and through every
// kind of scope on the way: a child instance, a child of that child, a nested
// module (LRM 23.4), a generate block (LRM 27.4), an instance array (LRM 23.3.2),
// and a write through such a name lands in that instance's own copy and in no
// other's.
module Leaf #(parameter int ID = 0);
  int held = ID;
  initial #1 held = held + 100;
endmodule

module Mid;
  Leaf #(.ID(3)) leaf ();
  module Inner;
    int inner_held = 4;
    initial #1 inner_held = 40;
  endmodule
  Inner inner ();
  for (genvar i = 0; i < 2; i++) begin : g
    Leaf #(.ID(10 + i)) u ();
  end
  Leaf #(.ID(7)) row[2] ();
endmodule

module Outer;
  Mid mid ();
endmodule

module Top;
  Outer first ();
  Outer second ();
  Outer third ();

  initial #2 begin
    second.mid.leaf.held = -1;
    third.mid.inner.inner_held = -2;
    second.mid.g[1].u.held = -3;
    third.mid.row[1].held = -4;
  end

  final begin
    if (first.mid.leaf.held !== 103)
      $fatal(1, "first.mid.leaf.held was %0d, expected 103", first.mid.leaf.held);
    if (second.mid.leaf.held !== -1)
      $fatal(1, "second.mid.leaf.held was %0d, expected -1", second.mid.leaf.held);
    if (third.mid.leaf.held !== 103)
      $fatal(1, "third.mid.leaf.held was %0d, expected 103", third.mid.leaf.held);
    if (first.mid.inner.inner_held !== 40)
      $fatal(1, "first.mid.inner.inner_held was %0d, expected 40",
             first.mid.inner.inner_held);
    if (third.mid.inner.inner_held !== -2)
      $fatal(1, "third.mid.inner.inner_held was %0d, expected -2",
             third.mid.inner.inner_held);
    if (first.mid.g[1].u.held !== 111)
      $fatal(1, "first.mid.g[1].u.held was %0d, expected 111", first.mid.g[1].u.held);
    if (second.mid.g[1].u.held !== -3)
      $fatal(1, "second.mid.g[1].u.held was %0d, expected -3", second.mid.g[1].u.held);
    if (third.mid.g[1].u.held !== 111)
      $fatal(1, "third.mid.g[1].u.held was %0d, expected 111", third.mid.g[1].u.held);
    if (first.mid.row[1].held !== 107)
      $fatal(1, "first.mid.row[1].held was %0d, expected 107", first.mid.row[1].held);
    if (third.mid.row[1].held !== -4)
      $fatal(1, "third.mid.row[1].held was %0d, expected -4", third.mid.row[1].held);
    if (second.mid.row[1].held !== 107)
      $fatal(1, "second.mid.row[1].held was %0d, expected 107", second.mid.row[1].held);
    $display("All checks passed");
  end
endmodule

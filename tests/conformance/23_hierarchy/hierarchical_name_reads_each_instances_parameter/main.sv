// A hierarchical name reaching a parameter reads the value of the instance it
// lands in (LRM 23.6, 23.8), so two instances of one module given different
// values each read their own: a child reading its parent's parameter by an
// upward name, and a parent reading its child's by a downward one. Neither
// value sizes anything, so each module is one body for every value it is given.
module Leaf;
  int seen;
  initial seen = Holder.K;
endmodule

module Holder #(parameter int K = 0);
  Leaf leaf ();
  int seen_in_child;
  Inner #(.V(K * 10)) inner ();
  initial #1 seen_in_child = inner.V;
endmodule

module Inner #(parameter int V = 0);
endmodule

module Top;
  Holder #(.K(1)) one ();
  Holder #(.K(2)) two ();

  final begin
    if (one.leaf.seen !== 1) $fatal(1, "one.leaf read %0d", one.leaf.seen);
    if (two.leaf.seen !== 2) $fatal(1, "two.leaf read %0d", two.leaf.seen);
    if (one.seen_in_child !== 10)
      $fatal(1, "one read its child's %0d", one.seen_in_child);
    if (two.seen_in_child !== 20)
      $fatal(1, "two read its child's %0d", two.seen_in_child);
    $display("All checks passed");
  end
endmodule

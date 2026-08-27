// A head that none of the scopes enclosing the reference declares is looked
// for in the scope the reference's module was instantiated in, and the search
// carries on upward level by level from there (LRM 23.8). What that finds is a
// scope beside the reference's own rather than above it: a sibling generate
// block, a sibling instance, or a sibling of some ancestor several levels up.
// Two sibling instances of one module are told apart by their instance names,
// and an iteration of a loop generate reaches its own iteration and any other
// by index, an index later than its own included. Every scope reachable this
// way holds a different value, so a reference that bound to a neighbour would
// be visible.
module Foo;
  int v;
endmodule

module Leaf;
  int from_ancestor_sibling;

  always_comb from_ancestor_sibling = my_foo.v;
endmodule

module Mid;
  Leaf leaf();
endmodule

module Top;
  if (1) begin : a
    int ax;
    int from_b;
    always_comb from_b = b.bx;
  end

  if (1) begin : b
    int bx;
    int from_a;
    always_comb from_a = a.ax;
  end

  if (1) begin : reader
    int from_foo_a;
    int from_foo_b;
    always_comb from_foo_a = foo_a.v;
    always_comb from_foo_b = foo_b.v;
  end

  Foo my_foo();
  Mid mid();
  Foo foo_a();
  Foo foo_b();

  for (genvar i = 0; i < 3; i = i + 1) begin : ring
    int v = (i + 1) * 4;
    int own;
    int fwd;
    always_comb own = ring[i].v;
    always_comb fwd = ring[(i + 1) % 3].v;
  end

  initial begin
    a.ax = 33;
    b.bx = 7;
    my_foo.v = 22;
    foo_a.v = 11;
    foo_b.v = 99;
  end

  final begin
    if (a.from_b !== 7) $fatal(1, "a.from_b was %0d, expected 7", a.from_b);
    if (b.from_a !== 33) $fatal(1, "b.from_a was %0d, expected 33", b.from_a);
    if (mid.leaf.from_ancestor_sibling !== 22)
      $fatal(1, "mid.leaf.from_ancestor_sibling was %0d, expected 22",
             mid.leaf.from_ancestor_sibling);
    if (reader.from_foo_a !== 11)
      $fatal(1, "reader.from_foo_a was %0d, expected 11", reader.from_foo_a);
    if (reader.from_foo_b !== 99)
      $fatal(1, "reader.from_foo_b was %0d, expected 99", reader.from_foo_b);

    if (ring[0].own !== 4)
      $fatal(1, "ring[0].own was %0d, expected 4", ring[0].own);
    if (ring[1].own !== 8)
      $fatal(1, "ring[1].own was %0d, expected 8", ring[1].own);
    if (ring[2].own !== 12)
      $fatal(1, "ring[2].own was %0d, expected 12", ring[2].own);
    if (ring[0].fwd !== 8)
      $fatal(1, "ring[0].fwd was %0d, expected 8", ring[0].fwd);
    if (ring[1].fwd !== 12)
      $fatal(1, "ring[1].fwd was %0d, expected 12", ring[1].fwd);
    if (ring[2].fwd !== 4)
      $fatal(1, "ring[2].fwd was %0d, expected 4", ring[2].fwd);
    $display("All checks passed");
  end
endmodule

// One elaboration covers every shape of LRM 23.8 step b/c visible-child
// resolution -- semantically symmetric across slang's `isUpward` flag, so
// both directions of the same generate-sibling read appear here:
//
//   - Shallow sibling generate, slang-upward (block `a` reads `b.bx == 7`).
//     `a` is declared before `b`, so slang's lexical resolution walks out
//     to find `b`; the install runs at resolve time and reaches the sibling
//     scope through the runtime SDK's visible-child climb.
//
//   - Shallow sibling generate, slang-downward (block `b` reads
//     `a.ax == 33`). `a` is already visible at `b`'s lexical resolution,
//     so slang flags it downward with the head in an enclosing scope; the
//     install climbs the typed parent edge (`kParent` + `PointerCastExpr`)
//     and composes a typed `MemberAccess` chain into the head's owned
//     child. Same semantic target as the upward case, different install
//     mechanism driven only by slang's source-order classification.
//
//   - Sibling-of-ancestor at depth (Leaf reads `my_foo.v == 22`). From
//     inside `mid.leaf`, my_foo is neither on the chain nor in a closer
//     enclosing scope -- the climb scans Top's children to find it.
//
//   - Two siblings of the same module distinguished by canonical instance
//     name. `reader` (declared before foo_a/foo_b so the references are
//     forward, upward) reads `foo_a.v == 11` and `foo_b.v == 99`; a
//     def-name-based climb would collapse both onto the same Foo.
//
//   - Indexed loop-generate iterations reading each other. `ring[i].own` reads
//     its own `ring[i].v` -- the constant index selects the same heterogeneous
//     member; `ring[i].fwd` reads `ring[(i+1)%3].v`, a wrap that includes a
//     forward read of a later-built sibling, which resolves only because the
//     reference binds after the whole tree is built. Asserting the self and
//     forward routes separately keeps a coincidental total from hiding either.
module Foo;
  int v;
endmodule

module Leaf;
  int from_top_my_foo;
  always_comb from_top_my_foo = my_foo.v;
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
    int from_a;
    int from_b;
    always_comb from_a = foo_a.v;
    always_comb from_b = foo_b.v;
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
    b.bx = 7;
    a.ax = 33;
    my_foo.v = 22;
    foo_a.v = 11;
    foo_b.v = 99;
    #1;
    $display("up=%0d down=%0d deep=%0d a=%0d b_inst=%0d own=%0d,%0d,%0d fwd=%0d,%0d,%0d",
             a.from_b, b.from_a, mid.leaf.from_top_my_foo,
             reader.from_a, reader.from_b,
             ring[0].own, ring[1].own, ring[2].own,
             ring[0].fwd, ring[1].fwd, ring[2].fwd);
  end
endmodule

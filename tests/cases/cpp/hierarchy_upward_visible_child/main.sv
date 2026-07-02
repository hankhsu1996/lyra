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

  initial begin
    b.bx = 7;
    a.ax = 33;
    my_foo.v = 22;
    foo_a.v = 11;
    foo_b.v = 99;
    #1;
    $display("up=%0d down=%0d deep=%0d a=%0d b_inst=%0d",
             a.from_b, b.from_a, mid.leaf.from_top_my_foo,
             reader.from_a, reader.from_b);
  end
endmodule

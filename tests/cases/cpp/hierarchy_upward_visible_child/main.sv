// One elaboration covers every shape of LRM 23.8 step b/c upward
// visible-child resolution. The runtime climb walks each enclosing level
// and matches a child by its canonical instance name; this case bites
// every code path that walk exercises, with values chosen so a wrong
// lookup observably fails:
//
//   - Shallow sibling generate (block `a` reads `b.bx == 7`). `a` is
//     declared before `b`, so slang classifies the reference as upward
//     (the converse `b -> a` rides slang's downward path, an unrelated
//     lowering limitation tracked separately).
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
    int from_b;
    always_comb from_b = b.bx;
  end
  if (1) begin : b
    int bx;
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
    my_foo.v = 22;
    foo_a.v = 11;
    foo_b.v = 99;
    #1;
    $display("shallow=%0d deep=%0d a=%0d b_inst=%0d",
             a.from_b, mid.leaf.from_top_my_foo,
             reader.from_a, reader.from_b);
  end
endmodule

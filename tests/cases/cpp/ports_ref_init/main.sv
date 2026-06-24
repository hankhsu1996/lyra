// A child may read its `ref` port in a variable initializer (LRM 6.8), which
// runs at time zero. The port must already be bound to the connected variable
// at that point: variable initialization is a phase that runs after the whole
// tree's references are resolved, so the read sees the connected cell rather
// than an unbound reference. `shared` has no initializer, so it holds its data
// type's default (0) when `captured` reads it, independent of cross-module
// initialization order.
module Child(ref int r);
  int captured = r;
  final $display("captured=%0d", captured);
endmodule

module Top;
  int shared;
  Child c(.r(shared));
endmodule

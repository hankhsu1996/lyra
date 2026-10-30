// An interface can be passed through ports (LRM 25.3), so a module's own
// interface port is a legal actual for a child's interface port and every port
// on the chain denotes the one interface instance the outermost connection
// named. A write at the far end of the chain is therefore a write to that
// instance's own variable, visible to every scope holding it, and a module on a
// chain that started at a different instance sees nothing of it.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Leaf (
    Bus b
);
  logic [7:0] seen = 8'h3c;

  initial #2 begin
    seen  = b.addr;
    b.hits = b.hits + 1;
  end
endmodule

module Mid (
    Bus m
);
  logic [7:0] mirror = 8'h3c;

  Leaf leaf (m);

  initial #3 mirror = m.addr;
endmodule

module Outer (
    Bus o
);
  Mid mid (o);
endmodule

module Top;
  Bus shared ();
  Bus other ();

  Outer deep (shared);
  Mid shallow (other);

  initial #1 begin
    shared.addr = 8'ha5;
    other.addr  = 8'h5a;
  end

  final begin
    if (deep.mid.leaf.seen !== 8'ha5)
      $fatal(1, "deep.mid.leaf.seen was %h, expected a5", deep.mid.leaf.seen);
    if (deep.mid.mirror !== 8'ha5)
      $fatal(1, "deep.mid.mirror was %h, expected a5", deep.mid.mirror);
    if (shallow.leaf.seen !== 8'h5a)
      $fatal(1, "shallow.leaf.seen was %h, expected 5a", shallow.leaf.seen);
    if (shared.hits !== 1)
      $fatal(1, "shared.hits was %0d, expected 1", shared.hits);
    if (other.hits !== 1)
      $fatal(1, "other.hits was %0d, expected 1", other.hits);
    $display("All checks passed");
  end
endmodule

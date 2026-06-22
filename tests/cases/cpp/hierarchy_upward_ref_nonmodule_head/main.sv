// An upward reference whose head is not a module instance: a `$root`-anchored
// absolute path (LRM 23.6) and a named generate block (LRM 23.8). Both heads
// name a scope that already exists in the object tree, so the reference reads
// the leaf by name like any other hierarchical reference. Leaf sits under Mid
// under generate block `blk` under Top, so `blk` is two hops up. The reference
// reaches a signal directly on the head, descends a by-name tail past it, and
// is written as well as read.
module Leaf;
  int a, b, c, d;
  always_comb a = $root.Top.g;       // root anchor, leaf a level down
  always_comb b = blk.bg;            // generate-block head, leaf on the block
  always_comb c = blk.m.ms;          // generate-block head, by-name tail past it
  always_comb d = $root.Top.blk.bg;  // root anchor, path through a generate block
  initial begin
    #1;
    $display("a=%0d b=%0d c=%0d d=%0d", a, b, c, d);
    blk.bg = 9;  // write through a generate-block head
  end
endmodule

module Mid;
  int ms;
  Leaf l();
  initial ms = 3;
endmodule

module Top;
  int g;
  if (1) begin : blk
    int bg;
    Mid m();
    initial bg = 5;
  end
  initial begin
    g = 7;
    #2;
    $display("top g=%0d blk.bg=%0d", g, blk.bg);
  end
endmodule

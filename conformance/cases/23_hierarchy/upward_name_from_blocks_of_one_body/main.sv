// A hierarchical name searched upward resolves in each instance to the nearest
// enclosing scope it matches (LRM 23.8), whatever block of the instance's body
// it is written in: at the body's top, two blocks down, or in every block a
// loop generate builds (LRM 27.4). So one module instantiated under two
// different modules reads each one's own declaration from all of them.
module Leaf;
  int at_top;
  int nested;
  int counted[3];

  initial #1 at_top = p.x;

  if (1) begin : outer
    if (1) begin : inner
      initial #1 nested = p.x + 1;
    end
  end

  for (genvar i = 0; i < 3; i++) begin : g
    initial #1 counted[i] = p.x + i;
  end
endmodule

module First;
  int x = 5;
  Leaf l ();
endmodule

module Second;
  int x = 9;
  Leaf l ();
endmodule

module Top;
  First p ();
  if (1) begin : b
    Second p ();
  end

  final begin
    if (p.l.at_top !== 5) $fatal(1, "p.l.at_top was %0d, expected 5", p.l.at_top);
    if (p.l.nested !== 6) $fatal(1, "p.l.nested was %0d, expected 6", p.l.nested);
    if (p.l.counted[2] !== 7)
      $fatal(1, "p.l.counted[2] was %0d, expected 7", p.l.counted[2]);
    if (b.p.l.at_top !== 9)
      $fatal(1, "b.p.l.at_top was %0d, expected 9", b.p.l.at_top);
    if (b.p.l.nested !== 10)
      $fatal(1, "b.p.l.nested was %0d, expected 10", b.p.l.nested);
    if (b.p.l.counted[2] !== 11)
      $fatal(1, "b.p.l.counted[2] was %0d, expected 11", b.p.l.counted[2]);
    $display("All checks passed");
  end
endmodule

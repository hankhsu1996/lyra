// A bind naming one instance inserts its instantiation into that instance
// only, and one naming a module with an instance list inserts it into the
// listed instances only (LRM 23.11). The bound instantiation's names are read
// from the target's point of view, so two binds of one instance name into two
// targets, connected to different names, each connect their own.
module Probe (input int seen, output int hit);
  int got = -1;
  initial #1 got = seen;
  assign hit = 1;
endmodule

module Leaf;
  int one = 1;
  int two = 2;
  int hit;
endmodule

module Mid;
  Leaf l ();
endmodule

module Top;
  Mid m1 (), m2 ();
  bind Top.m1.l Probe p (.seen(one), .hit(hit));

  Leaf x (), y (), z ();
  bind Leaf : x, z Probe q (.seen(two), .hit(hit));

  Leaf a (), b ();
  bind Top.a Probe r (.seen(one), .hit(hit));
  bind Top.b Probe r (.seen(two), .hit(hit));

  final begin
    if (m1.l.p.got !== 1) $fatal(1, "m1.l.p saw %0d", m1.l.p.got);
    if (m1.l.hit !== 1) $fatal(1, "m1.l was not bound into");
    if (m2.l.hit !== 0) $fatal(1, "m2.l was bound into");
    if (x.q.got !== 2 || z.q.got !== 2)
      $fatal(1, "x.q saw %0d and z.q %0d", x.q.got, z.q.got);
    if (x.hit !== 1 || z.hit !== 1) $fatal(1, "a listed target was missed");
    if (y.hit !== 0) $fatal(1, "y was bound into though not listed");
    if (a.r.got !== 1) $fatal(1, "a.r saw %0d", a.r.got);
    if (b.r.got !== 2) $fatal(1, "b.r saw %0d", b.r.got);
    $display("All checks passed");
  end
endmodule

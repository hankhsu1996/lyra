// An interface port instance is connected to an interface instance or to a
// higher level interface port (LRM 23.3.3.4), so a module whose own port
// carries a range hands that range on to a child the same way an instance array
// is handed on. The port and what is connected to it have the same number of
// dimensions and the same size in each, and their elements are paired left
// index to left index, right index to right index (LRM 23.3.3.5) -- so the
// pairing follows the two declared ranges and not the coordinates either side
// writes. Handing on one element of a ranged port, a contiguous part of it, or
// the whole of it are the same act at different widths, and a port restricted
// to a modport view (LRM 25.5) forwards under the view it was given.
interface Bus;
  int v;
  modport tap (output v);
endinterface

// Writes by declared coordinate, whichever way its own port was declared.
module Row (Bus p[1:0]);
  int seen_left = -1;
  int seen_right = -1;

  initial #1 begin
    seen_left  = p[1].v;
    seen_right = p[0].v;
    p[1].v     = 11;
    p[0].v     = 10;
  end
endmodule

// The forwarding port ascends where the child's descends, so the two are paired
// by position from the left and not by the coordinate each writes.
module MidRow (Bus p[0:1]);
  Row r (.p(p));
endmodule

module Grid (Bus p[2][3]);
  initial #1 begin
    p[0][0].v = 100;
    p[1][2].v = 112;
  end
endmodule

module MidGrid (Bus p[2][3]);
  Grid g (.p(p));
endmodule

module Plain (Bus p[3]);
  initial #1 p[2].v = 32;
endmodule

// One coordinate of a two-dimensional port leaves a one-dimensional one.
module MidRank (Bus p[2][3]);
  Plain r (.p(p[1]));
endmodule

module Pair (Bus p[0:1]);
  initial #1 begin
    p[0].v = 50;
    p[1].v = 51;
  end
endmodule

// A contiguous part of the port, which stands for as many instances as the part
// has elements and for no others.
module MidSlice (Bus p[0:3]);
  Pair q (.p(p[1:2]));
endmodule

module Deep (Bus p[2]);
  initial #1 begin
    p[0].v = 20;
    p[1].v = 21;
  end
endmodule

module MidInner (Bus p[2]);
  Deep d (.p(p));
endmodule

// A port forwarded twice is still the objects the outermost connection bound.
module MidOuter (Bus p[2]);
  MidInner m (.p(p));
endmodule

module Tap (Bus.tap p[2]);
  initial #1 begin
    p[0].v = 60;
    p[1].v = 61;
  end
endmodule

module MidTap (Bus.tap p[2]);
  Tap t (.p(p));
endmodule

module Top;
  Bus up[0:1] ();
  Bus grid[2][3] ();
  Bus rank[2][3] ();
  Bus quad[0:3] ();
  Bus chain[2] ();
  Bus taps[2] ();

  MidRow mid_row (.p(up));
  MidGrid mid_grid (.p(grid));
  MidRank mid_rank (.p(rank));
  MidSlice mid_slice (.p(quad));
  MidOuter mid_outer (.p(chain));
  MidTap mid_tap (.p(taps));

  initial begin
    up[0].v      = 70;
    up[1].v      = 71;
    grid[0][0].v = -1;
    grid[1][2].v = -1;
    rank[1][2].v = -1;
    quad[0].v    = 40;
    quad[1].v    = -1;
    quad[2].v    = -1;
    quad[3].v    = 43;
    chain[0].v   = -1;
    chain[1].v   = -1;
    taps[0].v    = -1;
    taps[1].v    = -1;
  end

  final begin
    // Row's leftmost element is MidRow's leftmost, so Row reads and writes
    // up[0] through its own p[1].
    if (mid_row.r.seen_left !== 70)
      $fatal(1, "seen_left was %0d, expected 70", mid_row.r.seen_left);
    if (mid_row.r.seen_right !== 71)
      $fatal(1, "seen_right was %0d, expected 71", mid_row.r.seen_right);
    if (up[0].v !== 11) $fatal(1, "up[0].v was %0d, expected 11", up[0].v);
    if (up[1].v !== 10) $fatal(1, "up[1].v was %0d, expected 10", up[1].v);

    if (grid[0][0].v !== 100)
      $fatal(1, "grid[0][0].v was %0d, expected 100", grid[0][0].v);
    if (grid[1][2].v !== 112)
      $fatal(1, "grid[1][2].v was %0d, expected 112", grid[1][2].v);

    if (rank[1][2].v !== 32)
      $fatal(1, "rank[1][2].v was %0d, expected 32", rank[1][2].v);

    if (quad[1].v !== 50) $fatal(1, "quad[1].v was %0d, expected 50", quad[1].v);
    if (quad[2].v !== 51) $fatal(1, "quad[2].v was %0d, expected 51", quad[2].v);
    if (quad[0].v !== 40) $fatal(1, "quad[0].v was %0d, expected 40", quad[0].v);
    if (quad[3].v !== 43) $fatal(1, "quad[3].v was %0d, expected 43", quad[3].v);

    if (chain[0].v !== 20)
      $fatal(1, "chain[0].v was %0d, expected 20", chain[0].v);
    if (chain[1].v !== 21)
      $fatal(1, "chain[1].v was %0d, expected 21", chain[1].v);

    if (taps[0].v !== 60)
      $fatal(1, "taps[0].v was %0d, expected 60", taps[0].v);
    if (taps[1].v !== 61)
      $fatal(1, "taps[1].v was %0d, expected 61", taps[1].v);

    $display("All checks passed");
  end
endmodule

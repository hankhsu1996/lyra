// Blocks of one loop whose nested conditionals select different alternatives
// (LRM 27.4, 27.5), reached from outside by names written once per alternative
// (LRM 23.6). The outer blocks alternate which alternative stands, an
// alternative whose width follows the index differs at every block that built
// it, and an alternative's text that is invalid where it was not selected is
// never built there. Two instances of one module handed values selecting
// different alternatives are reached the same way, and so is a declaration of
// a different type under one label, reached by one text written in two places
// whose child chose differently (LRM 23.8), or reached through an instance a
// block holds, of a unit the block's type parameter tells apart. A name landing
// on scopes of two classes and passing one block in an instance below reaches
// it through each class.
module Leaf #(parameter int K = 0);
  if (K % 2 == 0) begin : even
    int v;
    initial v = 100 + K;
  end else begin : odd
    int w;
    initial w = 200 + K;
  end
endmodule

module Typed #(parameter int K = 0);
  if (K % 2 == 0) begin : typed
    int v = 3;
  end else begin : typed
    real v = 2.5;
  end
endmodule

module ReadsBelow;
  real seen = -1.0;
  initial #1 seen = leaf.typed.v;
endmodule

module TypedBy #(type T = int);
  T v = T'(2.5);
endmodule

module ReadsThrough;
  real seen = -1.0;
  initial #1 seen = blk.leaf.v;
endmodule

module Holder #(parameter int K = 0);
  Typed #(.K(K)) leaf ();
  ReadsBelow r ();
endmodule

module ReadsTheChild;
  int seen = -1;
  initial #1 seen = Padded.u.even.v;
endmodule

module Padded #(parameter int W = 1);
  logic [W:0] pad;
  Leaf #(.K(0)) u ();
  ReadsTheChild r ();
endmodule

module Reader;
  int t1, f2, sized3, leaf_even, leaf_odd;
  initial begin
    #1;
    t1 = Top.gg[0].gi[1].t.x;
    f2 = Top.gg[1].gi[2].f.y;
    sized3 = Top.gg[3].gi[0].f.sized;
    leaf_even = Top.leaves[2].u.even.v;
    leaf_odd = Top.leaves[3].u.odd.w;
  end
endmodule

module Top;
  for (genvar g = 0; g < 4; g += 1) begin : gg
    for (genvar i = 0; i < 3; i += 1) begin : gi
      if (g % 2 == 0) begin : t
        int x;
        initial x = g * 10 + i;
      end else begin : f
        int y;
        logic [g:0] sized;
        initial begin
          y = g * 10 + i;
          sized = '1;
        end
      end
    end
    int carried;
    if (g == 0) begin : first
      assign carried = 1;
    end else begin : rest
      assign carried = gg[g-1].carried + 1;
    end
  end

  for (genvar k = 0; k < 4; k += 1) begin : leaves
    Leaf #(.K(k)) u ();
  end

  for (genvar j = 0; j < 2; j += 1) begin : through
    if (j == 0) begin : blk
      TypedBy #(.T(int)) leaf ();
    end else begin : blk
      TypedBy #(.T(real)) leaf ();
    end
    ReadsThrough r ();
  end

  Reader reader ();
  Padded #(.W(1)) narrow ();
  Padded #(.W(64)) wide ();
  Holder #(.K(0)) holds_int ();
  Holder #(.K(1)) holds_real ();

  final begin
    if (reader.t1 !== 1) $fatal(1, "gg[0].gi[1].t.x read %0d", reader.t1);
    if (reader.f2 !== 12) $fatal(1, "gg[1].gi[2].f.y read %0d", reader.f2);
    if (reader.sized3 !== 15)
      $fatal(1, "gg[3].gi[0].f.sized read %0d", reader.sized3);
    if (gg[3].carried !== 4)
      $fatal(1, "the chain of neighbours carried %0d", gg[3].carried);
    if (reader.leaf_even !== 102)
      $fatal(1, "leaves[2].u.even.v read %0d", reader.leaf_even);
    if (reader.leaf_odd !== 203)
      $fatal(1, "leaves[3].u.odd.w read %0d", reader.leaf_odd);
    if (holds_int.r.seen != 3.0)
      $fatal(1, "holds_int.r read %f", holds_int.r.seen);
    if (holds_real.r.seen != 2.5)
      $fatal(1, "holds_real.r read %f", holds_real.r.seen);
    if (narrow.r.seen !== 100) $fatal(1, "narrow.r read %0d", narrow.r.seen);
    if (wide.r.seen !== 100) $fatal(1, "wide.r read %0d", wide.r.seen);
    if (through[0].r.seen != 3.0)
      $fatal(1, "through[0].r read %f", through[0].r.seen);
    if (through[1].r.seen != 2.5)
      $fatal(1, "through[1].r read %f", through[1].r.seen);
    $display("All checks passed");
  end
endmodule

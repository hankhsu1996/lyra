// A name `scope_name.item_name` is searched for in the scope holding the
// instantiation and then in each scope enclosing that one (LRM 23.8), and a
// generate block is one of those scopes. So a module instantiated inside a
// block of a loop reaches, by the block's own name, the block instance that
// holds it and no other (LRM 27.3 makes each block instance a level of
// hierarchy) -- whatever it reads there: a variable, a constant the block
// declares from its index, a declaration of whichever type the alternative
// built at that index gave it (LRM 27.5) -- and so does an instance below one
// that writes such a name itself.
module Leaf;
  int seen = -1;

  initial begin
    #1;
    seen = inner.held;
    inner.answered = inner.held + 1000;
  end
endmodule

module ReadsConstant;
  int seen = -1;

  initial seen = inner.K;
endmodule

module Wrapper;
  int observed = -1;

  initial #1 observed = inner.held;

  ReadsConstant c ();
endmodule

module ReadsTyped;
  real seen = -1.0;

  initial #1 seen = typed.v;
endmodule

module Top;
  for (genvar i = 0; i < 3; i++) begin : g
    if (1) begin : inner
      localparam int K = (i + 1) * 7;
      int held = -1;
      int answered = -1;
      Leaf u ();
      Wrapper w ();
      initial held = (i + 1) * 10;
    end
  end

  for (genvar j = 0; j < 2; j++) begin : h
    if (j == 0) begin : typed
      int v = 3;
      ReadsTyped r ();
    end else begin : typed
      real v = 2.5;
      ReadsTyped r ();
    end
  end

  final begin
    if (g[0].inner.u.seen !== 10)
      $fatal(1, "g[0].inner.u read %0d, expected 10", g[0].inner.u.seen);
    if (g[1].inner.u.seen !== 20)
      $fatal(1, "g[1].inner.u read %0d, expected 20", g[1].inner.u.seen);
    if (g[2].inner.u.seen !== 30)
      $fatal(1, "g[2].inner.u read %0d, expected 30", g[2].inner.u.seen);
    if (g[1].inner.answered !== 1020)
      $fatal(1, "g[1].inner was written %0d, expected 1020", g[1].inner.answered);
    if (g[2].inner.answered !== 1030)
      $fatal(1, "g[2].inner was written %0d, expected 1030", g[2].inner.answered);
    if (g[0].inner.w.c.seen !== 7)
      $fatal(1, "g[0].inner.w.c read %0d, expected 7", g[0].inner.w.c.seen);
    if (g[1].inner.w.c.seen !== 14)
      $fatal(1, "g[1].inner.w.c read %0d, expected 14", g[1].inner.w.c.seen);
    if (g[2].inner.w.c.seen !== 21)
      $fatal(1, "g[2].inner.w.c read %0d, expected 21", g[2].inner.w.c.seen);
    if (g[0].inner.w.observed !== 10)
      $fatal(1, "g[0].inner.w read %0d, expected 10", g[0].inner.w.observed);
    if (g[1].inner.w.observed !== 20)
      $fatal(1, "g[1].inner.w read %0d, expected 20", g[1].inner.w.observed);
    if (g[2].inner.w.observed !== 30)
      $fatal(1, "g[2].inner.w read %0d, expected 30", g[2].inner.w.observed);
    if (h[0].typed.r.seen != 3.0)
      $fatal(1, "h[0].typed.r read %f, expected 3.0", h[0].typed.r.seen);
    if (h[1].typed.r.seen != 2.5)
      $fatal(1, "h[1].typed.r read %f, expected 2.5", h[1].typed.r.seen);
    $display("All checks passed");
  end
endmodule

// A name `scope_name.item_name` is searched for in the scope holding the
// instantiation and then in each scope enclosing that one (LRM 23.8), and a
// generate block is one of those scopes. So a module instantiated inside a
// block of a loop reaches, by the block's own name, the block instance that
// holds it and no other (LRM 27.3 makes each block instance a level of
// hierarchy).
module Leaf;
  int seen = -1;

  initial begin
    #1;
    seen = inner.held;
    inner.answered = inner.held + 1000;
  end
endmodule

module Top;
  for (genvar i = 0; i < 3; i++) begin : g
    if (1) begin : inner
      int held = -1;
      int answered = -1;
      Leaf u ();
      initial held = (i + 1) * 10;
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
    $display("All checks passed");
  end
endmodule

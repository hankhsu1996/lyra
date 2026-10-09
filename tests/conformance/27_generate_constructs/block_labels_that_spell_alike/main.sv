// An escaped identifier may hold any printable character (LRM 5.6.1), so a
// generate block's label may hold the characters a hierarchical path is
// written with. Each block is still its own scope under its own label (LRM
// 27.6): a block labelled `\g::h ` and a block `h` inside a block `g` are two
// scopes, and so are a class `C` declared in a block `\a_b ` and a class
// `\b_C ` declared in a block `a`.
module Reader;
  // The name lands on the block holding this instance (LRM 23.8), which is a
  // different scope in each of the two places the module is instantiated.
  function automatic int Marked();
    return inner.mark;
  endfunction
endmodule

// Three declarations of one name that one block holds (LRM 23.9): the block's
// own, a static property of a class it declares (LRM 8.9), and a static
// variable of a named block in a function whose escaped name spells that
// class's scope.
module Counts;
  function automatic int OfBlock();
    return Top.g.count;
  endfunction
  function automatic int OfNamedBlock();
    return Top.g.\g::C .keep.count;
  endfunction
  function automatic int OfClass();
    return Top.g.held.count;
  endfunction
endmodule

// The alternatives of one conditional may share a label, since only one of
// them is built (LRM 27.5), and they are still two blocks: each instance holds
// the one its parameter chose. A block beside them whose escaped label spells
// a label and two numbers is a third.
module Chooser #(
    parameter bit P = 0
);
  if (P) begin : g
    int v = 31;
  end else begin : g
    int v = 32;
    int w = 33;
  end
  if (1) begin : \g#1.1
    int v = 34;
  end
  if (1) begin : \g#1.0
    int v = 35;
  end
endmodule

module Top;
  if (1) begin : \g::h
    int x = 1;
    if (1) begin : inner
      int mark = 7;
      Reader reader ();
    end
  end
  if (1) begin : g
    if (1) begin : h
      int y = 2;
      logic z = 1'b1;
      if (1) begin : inner
        int mark = 8;
        int more = 9;
        Reader reader ();
      end
    end

    class C;
      static int count = 21;
    endclass
    C held = new;
    function int \g::C ();
      begin : keep
        static int count = 22;
        return count;
      end
    endfunction
    int count = 23;
  end
  Counts counts ();
  Chooser #(.P(0)) took_else ();
  Chooser #(.P(1)) took_if ();

  // The blocks of one loop are told apart by what their index decides (LRM
  // 27.4), here the width of what each holds, and a block beside the loop
  // whose escaped label spells the loop's label and more is one more scope.
  for (genvar i = 1; i < 3; i++) begin : sized
    logic [i-1:0] bits = '1;
  end
  if (1) begin : \sized__08574c6645fe4deb
    int bits = 61;
    int more = 62;
  end

  if (1) begin : \a_b
    class C;
      int v = 3;
    endclass
    C held = new;
  end
  if (1) begin : a
    class \b_C ;
      int w = 4;
      int u = 5;
    endclass
    \b_C held = new;
  end

  // A block is a scope of its own, so it may carry the label of the block
  // holding it, to any depth.
  if (1) begin : same
    if (1) begin : same
      if (1) begin : same
        int deep = 6;
      end
    end
  end

  initial begin
    if (same.same.same.deep !== 6)
      $fatal(1, "the innermost block holds %0d", same.same.same.deep);
    if (\g::h .x !== 1) $fatal(1, "the escaped block holds %0d", \g::h .x);
    if (g.h.y !== 2) $fatal(1, "the nested block holds %0d", g.h.y);
    if (g.h.z !== 1'b1) $fatal(1, "the nested block's bit is %b", g.h.z);
    if (\g::h .inner.reader.Marked() !== 7)
      $fatal(1, "the escaped block's reader reads %0d",
             \g::h .inner.reader.Marked());
    if (g.h.inner.reader.Marked() !== 8)
      $fatal(1, "the nested block's reader reads %0d",
             g.h.inner.reader.Marked());
    if (\a_b .held.v !== 3) $fatal(1, "C holds %0d", \a_b .held.v);
    if (a.held.w !== 4) $fatal(1, "b_C holds %0d", a.held.w);
    if (a.held.u !== 5) $fatal(1, "b_C holds %0d second", a.held.u);
    if (counts.OfBlock() !== 23)
      $fatal(1, "the block's own count reads %0d", counts.OfBlock());
    if (counts.OfNamedBlock() !== 22)
      $fatal(1, "the named block's count reads %0d", counts.OfNamedBlock());
    if (counts.OfClass() !== 21)
      $fatal(1, "the class's count reads %0d", counts.OfClass());
    if (g.\g::C () !== 22) $fatal(1, "the function reads %0d", g.\g::C ());
    if (took_else.g.v !== 32)
      $fatal(1, "the else alternative holds %0d", took_else.g.v);
    if (took_else.g.w !== 33)
      $fatal(1, "the else alternative holds %0d second", took_else.g.w);
    if (took_if.g.v !== 31)
      $fatal(1, "the if alternative holds %0d", took_if.g.v);
    if (took_else.\g#1.1 .v !== 34)
      $fatal(1, "the first escaped block holds %0d", took_else.\g#1.1 .v);
    if (took_if.\g#1.1 .v !== 34)
      $fatal(1, "the first escaped block holds %0d", took_if.\g#1.1 .v);
    if (took_else.\g#1.0 .v !== 35)
      $fatal(1, "the second escaped block holds %0d", took_else.\g#1.0 .v);
    if (took_if.\g#1.0 .v !== 35)
      $fatal(1, "the second escaped block holds %0d", took_if.\g#1.0 .v);
    if (sized[1].bits !== 1'b1)
      $fatal(1, "the first sized block holds %b", sized[1].bits);
    if (sized[2].bits !== 2'b11)
      $fatal(1, "the second sized block holds %b", sized[2].bits);
    if (\sized__08574c6645fe4deb .bits !== 61)
      $fatal(1, "the block beside the loop holds %0d",
             \sized__08574c6645fe4deb .bits);
    if (\sized__08574c6645fe4deb .more !== 62)
      $fatal(1, "the block beside the loop holds %0d second",
             \sized__08574c6645fe4deb .more);
    $display("All checks passed");
  end
endmodule

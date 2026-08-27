// A named begin-end block defines a level of the name hierarchy, and a named
// block inside a named block branches from it, so a static declared in one is
// reached by the labels that lead to it (LRM 23.6). An unnamed begin-end block
// is a scope for the names used within it but contributes nothing to a path,
// so a named block wrapped in unnamed ones is reached by its own label as if
// the wrappers were not written. Such a path is available to a procedure
// beside the block in the same module and to one in another instance, whether
// the reference stands before or after the block in the source, and what it
// names may be written as well as read (LRM 23.6, 23.9).
module Child;
  int intra_outer;
  int intra_inner;

  initial begin
    #1;
    intra_outer = outer.x;
    intra_inner = outer.inner.y;
  end

  initial begin : outer
    static int x = 7;
    begin : inner
      static int y = 13;
    end
  end

  initial begin
    begin
      begin : deep
        static int d = 42;
      end
    end
  end
endmodule

module Top;
  Child c();

  int from_unnamed;
  int wrapped;

  initial begin : own
    static int v = 55;
    begin
      static int hidden = 61;
      from_unnamed = hidden + v;
    end
  end

  initial begin
    #2;
    wrapped = c.deep.d;
    c.outer.x = 100;
  end

  final begin
    if (c.intra_outer !== 7)
      $fatal(1, "c.intra_outer was %0d, expected 7", c.intra_outer);
    if (c.intra_inner !== 13)
      $fatal(1, "c.intra_inner was %0d, expected 13", c.intra_inner);
    if (from_unnamed !== 116)
      $fatal(1, "from_unnamed was %0d, expected 116", from_unnamed);
    if (own.v !== 55) $fatal(1, "own.v was %0d, expected 55", own.v);
    if (wrapped !== 42) $fatal(1, "wrapped was %0d, expected 42", wrapped);
    if (c.outer.x !== 100)
      $fatal(1, "c.outer.x was %0d, expected 100", c.outer.x);
    $display("All checks passed");
  end
endmodule

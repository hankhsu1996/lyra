// LRM 9.3.5 / 23.6 / 23.9: named begin/end is a hierarchical-reference
// head; unnamed begin/end is transparent in the hierarchical-name path.
// This exercises the placement and lookup behaviour that the primary
// D2e test (`hierarchy_named_block_head`) does not cover:
//
//   1. A static declared directly in an unnamed block nested inside a
//      named block. The static has no hierarchical name from outside;
//      internally the enclosing body reads it like any local.
//   2. A named begin/end wrapped in one or more unnamed begin/ends. LRM
//      lets a peer reach it by its label (`Top.c.deep.d`) as if the
//      unnamed wrappers did not exist -- the runtime tree must project
//      the same view.
//   3. A sibling procedural block in the same module reaching a named
//      block by its label (`Top.outer.a`), without a cross-instance
//      hop.

module Inner;
  initial begin
    begin
      begin : deep
        static int d = 42;
      end
    end
  end
endmodule

module Top;
  Inner c();

  initial begin : outer
    static int a = 7;
    begin
      static int b = 13;
      $display("outer internal a=%0d b=%0d", a, b);
    end
  end

  initial begin
    #1;
    $display("Top.outer.a=%0d", outer.a);
    $display("Top.c.deep.d=%0d", c.deep.d);
  end
endmodule

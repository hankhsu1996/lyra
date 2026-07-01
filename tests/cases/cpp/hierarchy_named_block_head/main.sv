// LRM 23.9: a named procedural block is a hierarchical-reference head.
// `Top.c.outer.x` and `Top.c.outer.inner.y` reach static-lifetime locals
// declared inside the named blocks of `Child`. A sibling process inside
// `Child` reads the same statics through an intra-unit typed segment
// (`outer.x`), and an external write through the hierarchical path is
// observed at a later time step.

module Child;
  int intra_x;
  int intra_inner_y;

  initial begin : outer
    static int x = 7;
    begin : inner
      static int y = 13;
    end
  end

  initial begin
    #1;
    intra_x = outer.x;
    intra_inner_y = outer.inner.y;
    $display("intra outer.x=%0d outer.inner.y=%0d", intra_x, intra_inner_y);
  end
endmodule

module Top;
  Child c();
  int xread;
  int yread;

  initial begin
    #2;
    xread = c.outer.x;
    yread = c.outer.inner.y;
    $display("x=%0d y=%0d", xread, yread);

    c.outer.x = 100;
    #1;
    $display("after write x=%0d", c.outer.x);
  end
endmodule

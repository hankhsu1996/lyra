// A class handle is a declaration kind a hierarchical name reaches, and its
// property is reached through the handle the path lands on (LRM 8.4, 23.7).
// A downward path reaches one today; an upward path names a class the
// enclosing module declares, which is a type the child's own artifact was
// never given -- a module publishes its parameters and ports and nothing else
// -- so this direction owes an answer the downward one already has.
module Child;
  int saw = 0;

  initial begin
    #1;
    saw = Top.handle.held;
  end
endmodule

module Top;
  class Cell;
    int held = 41;
  endclass

  Cell handle = new();

  Child c ();

  final begin
    if (c.saw !== 41)
      $fatal(1, "an upward class property read %0d, expected 41", c.saw);
    $display("All checks passed");
  end
endmodule
